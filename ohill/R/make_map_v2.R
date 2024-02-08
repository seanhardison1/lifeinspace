library(tidyverse)
library(osmdata)
library(smoothr)
library(sfnetworks)
library(ggplot2)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sp)
library(sf)
library(ggnewscale)
library(isoband)
library(raster)
library(geomtextpath)

load(here::here("data/ohill_dem_processed.rdata"))
source(here::here("R/query_osm.R"))
ncrs <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"

# create slope and hillshade
slope = terrain(ohill_dem_full, opt='slope')
aspect = terrain(ohill_dem_full, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill_df <- hill %>% 
  dream::rst_to_tib(var_name = "slope")

# get topography
ohill_cont <- rasterToContour(ohill_dem, nlevels = 25)

ohill_cont_sf <- as(ohill_cont, "sf") %>% 
  st_transform(ncrs) %>% 
  smoothr::smooth(., method = "ksmooth", smoothness = 10) %>%
  st_cast("LINESTRING") %>% 
  mutate(level = as.numeric(level),
         length = as.numeric(st_length(.)),
         id = 1:nrow(.)) %>% 
  filter(length > 100 | (length > 100 & level > 45))

t <- 
  sf::st_coordinates(ohill_cont_sf) %>% 
  as_tibble() %>% 
  left_join(.,ohill_cont_sf %>% 
              mutate(id = 1:nrow(.)) %>% 
              st_set_geometry(NULL),
            by = c("L1" = "id")) %>% 
  distinct() %>% 
  mutate(cont_lab = ifelse(level %in% seq(140, 270, 20),
                           "show","no show"))

ohill_cont2 <- dream::rst_to_tib(ohill_dem)

# make isobands
m <- as.matrix(ohill_dem)

x <- coordinates(ohill_dem) %>% 
  as.data.frame() %>% 
  pull(x) %>% 
  unique

y <- coordinates(ohill_dem) %>% 
  as.data.frame() %>% 
  pull(y) %>% 
  unique

b <- isobands(x, 
              y, 
              m, 
              10*(13:26), 10*(14:27))
bands <- iso_to_sfg(b)

ohill_sf <- 
  st_sf(
    level = 1:length(bands),
    geometry = st_sfc(bands,
                      crs = ncrs)
  ) %>% 
  smooth(., method = "ksmooth", smoothness = 4) %>%
  # st_transform(4326) %>% 
  mutate(bin = factor(ntile(level, 5)))

plot(ohill_sf)
# # contours
# ohill_sf <- as(ohill_cont, "sf") %>%
#   mutate(level = as.numeric(level),
#          levelf = factor(level)) %>%
#   smooth(., method = "ksmooth", smoothness = 4) %>%
#   st_transform(4326) %>% 
#   mutate(bin = factor(ntile(levelf, 5)))

# full dem
ohill_dem_df <- ohill_dem %>% 
  dream::rst_to_tib() %>% 
  mutate(bin = factor(ntile(fill_var, 5)))

# query OSM----
process <- F
if (process){
  osm_all_roads <- query_osm(key = "highway", bb = "ohill_sf")
  lines <- query_osm(key = "power", bb = "ohill_sf")
  natural <- query_osm(key = "natural", bb = "ohill_sf")
  natural2 <- query_osm(key = "landuse", bb = "ohill_sf")
  bridges <- query_osm(key = "bridge", select = "osm_lines",bb = "ohill_sf")
  access <- query_osm(key = "access", select = "osm_polygons",bb = "ohill_sf")
  waterway <- query_osm(key = "waterway",
                        bb = "ohill_sf")
  dt_build <- query_osm(key = "building",
                        bb = "ohill_sf")
  water <- query_osm(key = "water",
                     bb = "ohill_sf")
  away <- query_osm(key = "aerialway",
                    bb = "ohill_sf")
  rodeo <- query_osm(key = "sport",
                     bb = "ohill_sf")
  baseball <- query_osm(key = "leisure",
                        bb = "ohill_sf")
  place <- query_osm(key = "place",
                     bb = "ohill_sf")
  
  fp <- opq(bbox = st_bbox(ohill_sf)) %>%
    add_osm_feature(key = "highway", value = NULL) %>%
    osmdata_sf()
  
  stillfried_ln <- 
    fp$osm_polygons %>% 
    filter(name %in% c("Stillfried Lane") |
             osm_id == "20560204") %>% 
    st_cast("LINESTRING") 
  
  save(osm_all_roads,
       lines,
       baseball,
       natural,
       natural2,
       bridges,
       dt_build,
       access,
       waterway,
       water,
       stillfried_ln,
       away,
       rodeo,
       file = here::here("data/osm_data.rdata"))
} else {
  load(here::here("data/osm_data.rdata"))
}


osm_paths <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("path","footway","track","cycleway")) %>% 
  mutate(is_trail = ifelse(str_detect(name, "Trail"), "Trail","Road")) %>% 
  st_transform(ncrs)

np_large_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("motorway","motorway_link")) %>% 
  st_transform(ncrs)

primary_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("primary","trunk")) %>% 
  st_transform(ncrs)

tertiary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("unclassified","tertiary")) %>%
  st_transform(ncrs)

service <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("service","track")) %>% st_transform(ncrs)

residential <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("residential")) %>% 
  bind_rows(.,stillfried_ln) %>% st_transform(ncrs)

secondary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("secondary")) %>% st_transform(ncrs)

pks <- 
  natural$osm_points %>% 
  filter(natural == "peak") %>% 
  dplyr::select(name, ele) %>% 
  mutate(name = paste0(name,"\n", ele, "m")) %>% st_transform(ncrs)

trails <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("path","cycleway"),
         osm_id != "553814758")  %>% st_transform(ncrs)

footpaths <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("footway") | 
           osm_id == "553814758") %>% st_transform(ncrs)

snet1 <- 
  sfnetworks::st_network_join(sfnetworks::as_sfnetwork(np_large_roads),
                              sfnetworks::as_sfnetwork(primary_roads)) %>% 
  st_as_sf("edges")  %>% 
  smoothr::densify(.,n = 11) %>% 
  dplyr::select(highway) %>%
  st_transform(ncrs) %>%
  st_buffer(dist = 4.5,
            endCapStyle = "SQUARE") %>%
  mutate(grp = "large")

snet2 <- sfnetworks::st_network_join(sfnetworks::as_sfnetwork(secondary),
                                     sfnetworks::as_sfnetwork(tertiary)) %>% 
  st_as_sf("edges")  %>% 
  smoothr::densify(.,n = 10) %>% 
  dplyr::select(highway) %>% 
  st_transform(ncrs) %>% 
  st_buffer(dist = 4) %>%
  mutate(grp = "medium")  

snet3 <- sfnetworks::st_network_join(sfnetworks::as_sfnetwork(service),
                                     sfnetworks::as_sfnetwork(residential)) %>%
  st_as_sf("edges") %>% 
  dplyr::select(highway) %>% 
  smoothr::densify(.,n = 10) %>% 
  st_transform(ncrs) %>% 
  st_buffer(dist = 3) %>% 
  mutate(grp = "small") %>% 
  bind_rows(.,snet1) %>%
  bind_rows(.,snet2) %>%
  # st_intersection() %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_transform(ncrs)

snet4 <- smooth(snet3, method = "ksmooth", smoothness = 1)

test <- 
  ggplot() +
  geom_sf(data = snet4, size = 0.2, color = "grey20",
          fill = 'grey80')


pl <- lines$osm_lines %>% 
  st_transform(4326) %>%  
  dplyr::select(power) %>% st_transform(ncrs)

woods1 <- 
  natural2$osm_polygons %>% 
  st_transform(ncrs) %>% 
  filter(landuse %in% c("forest")) %>% 
  dplyr::select(natural = landuse) %>% st_transform(ncrs)

woods2 <- 
  natural$osm_polygons %>% 
  st_transform(ncrs) %>% 
  filter(natural %in% c("scrub","wood","forest")) %>% 
  dplyr::select(natural) %>% st_transform(ncrs)

woods <- rbind(woods1, woods2)

rivers <- 
  water$osm_polygons %>% 
  dplyr::select(osm_id) %>% 
  mutate(area = as.numeric(st_area(.))) %>% st_transform(ncrs)

other_water <- 
  natural$osm_polygons %>% 
  filter(natural == "water") %>% st_transform(ncrs)

stream <- 
  waterway$osm_lines %>% 
  filter(waterway == "stream") %>% 
  dplyr::select(waterway) %>% st_transform(ncrs)

buildings <- 
  dt_build$osm_polygons %>% 
  dplyr::select(osm_id, name) %>% st_transform(ncrs)

#map constants
dt_inset_color <- "black"
trail_color <- "indianred"
hs_alpha <- 0.5
mp <- 180


out <-
  ggplot() +
  # hill shade
  geom_raster(data = hill_df, aes(x = longitude, y = latitude, 
                                  fill = slope), show.legend = F) +
  scale_fill_gradient(low = "grey50", high = "white") +
  new_scale_fill()+
  # # dem
  # geom_raster(data = ohill_dem_df, aes(x = longitude, y = latitude,
  #                                      fill = fill_var),
  #             alpha = hs_alpha,
  #             show.legend = F) +
  # scale_fill_gradient2(low = "#cfbd9b",
  #                      mid = "white",
  #                      high = "#5a8c54",
  #                      midpoint = mp) +
  geom_sf(data = woods, fill = "#5a8c54", color = "transparent",alpha = 0.5) +
  # scale_fill_gradientn(colours = c("#1B9A49", "#b5a470", "#d9cba3")) +
  # "#91864a",
  # contours
  geom_path(data = t %>%
              filter(cont_lab == "no show"),
            aes(x = X, y = Y, group = L1,
                alpha = level),
            size = 0.1, padding  = unit(0, "pt"),
            color = "black") +
  # 
  geom_textpath(data = t %>%
                  filter(cont_lab == "show"),
                aes(x = X, y = Y, group = L1, label = level,
                    alpha = level),
                size = 2, padding  = unit(0, "pt"),
                linewidth = 0.2, color = "black") +
  # geom_sf(data = ohill_sf %>%
  #           st_transform(ncrs), aes(fill = level), size = 0.2,
  #         color = "grey50",
  #         show.legend = F,
  #         alpha = 0.7)+
  # scale_fill_gradientn(colors = c('#71b390',"#eee8c8","#f1a762","#e67d48")) +#"#25523b"
  # scale_fill_gradientn(colors = c("#bcc0ba","#aab5af","#9ba7a1",
  #                                 "#5f7a6b","#365544","#1b332d")) +
  # scale_fill_gradientn(colors = PNWColors::pnw_palette("Sunset",3)) +
  
  # water features
  geom_sf(data = rivers, color = "transparent", fill = "#78A2CC",
        size = 0.5) +
  geom_sf(data = stream, color = "#78A2CC",
          size = 0.25) +
  
  # trails
  geom_sf(data = trails, color = "white", size = 0.85) +
  geom_sf(data = trails, color = trail_color, size = 0.75) +
  geom_sf(data = trails, color = "#6e5e44", size = 0.65, lty = "11") +
  
  geom_sf(data = footpaths, color = "grey30", size = 0.3, lty = "11") +
  
  # roads
  geom_sf(data = snet3, size = 0.2, color = "grey50",
          fill = 'grey85') +
  # buildings
  geom_sf(data = buildings, size = 0.2, fill = "grey50") + 
  
  # power lines
  # geom_sf(data = pl, color = "grey", alpha = 0.9, size = 0.25) +
  # geom_sf(data = pl %>% st_cast("POINT"), color = "grey", alpha = 0.9,
  #         size = 0.125) +
  
  # theme
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  guides(alpha = "none",
         color= "none",
         fill = "none") +
  theme(rect = element_blank(),
        # panel.background = element_rect(fill = "white",
        #                                 color = NA),
        panel.grid= element_line(color = "grey30",
                                 size = 0.2),
        panel.ontop = TRUE,
        axis.ticks = element_blank(),
        # axis.text = element_blank(),
        axis.title = element_blank()) +
  coord_sf(xlim = c(716888,718500),
           ylim = c(4211593, 4214003),
           datum = st_crs(ncrs))

ggsave(out, filename = here::here("figs/test4_v2.tiff"),
       width = 12, 
       height = 18.55,
       units = "in")


trail_svg <- 
  ggplot() +
  geom_sf(data = trails, color = "white", size = 0.85) +
  geom_sf(data = trails, color = trail_color, size = 0.75) +
  geom_sf(data = trails, color = "#6e5e44", size = 0.65, lty = "11") +
  theme(rect = element_blank())



ggsave(trail_svg, filename = here::here("figs/ohill_trails_v2.svg"),
       width = 12, 
       height = 18.55,
       units = "in")

elev_grad_legend <- 
  ggplot() +
  # # dem
  geom_raster(data = ohill_dem_df, aes(x = longitude, y = latitude,
                                       fill = fill_var),
              alpha = hs_alpha) +
  scale_fill_gradientn(colours = alpha(c("#1B9A49", "#b5a470", "#d9cba3"), 0.75)) +
  theme(legend.direction = "horizontal")

ggsave(elev_grad_legend, filename = here::here("figs/ohill_elev_legened_v2.svg"),
       width = 12, 
       height = 18.55,
       units = "in")
