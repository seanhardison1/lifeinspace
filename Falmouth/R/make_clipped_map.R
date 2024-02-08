library(tidyverse)
library(osmdata)
library(smoothr)
library(sfnetworks)
library(elevatr)
library(ggplot2)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sp)
library(smoothr)
library(sf)
library(geomtextpath)
library(ggnewscale)
library(raster)
library(extrafont)
library(isoband)
library(shadowtext)

ncrs <- "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"

#set up------------------------------------------------------------------------
gis.dir <- here::here("Falmouth/gis")
r.dir <- here::here("Falmouth/R")
pdf.dir <- here::here("Falmouth/pdf")
fcwa_outer <- sf::st_read(here::here("Falmouth/gis/fcwa_poly2.kml"))


# map extent and DEM----
map_op <- fcwa_outer %>% 
  sf::st_zm() %>% 
  as_Spatial() 

map_extent <- extent(map_op)

dem <- raster::raster(here::here("falmouth/gis/USGS_13_n42w071_20191216.tif")) %>%
  crop(.,extent(map_op)) %>% 
  mask(.,map_op) %>% 
  raster::disaggregate(.,2.1, method = "bilinear") %>%
  raster::projectRaster(crs = crs(ncrs)) 
  
map_extent_sf <- map_op %>% as("sf") %>% st_transform(ncrs)
dem_df <- dream::rst_to_tib(dem)

#source OSM data--------------------------------------------------------------
source(file.path(r.dir,"query_osm.R"))
process <- T
if (process){
  #Query all roads
  osm_all_roads <- query_osm(key = "highway", bb = "map_extent")
  dt_build <- query_osm(key = "building",
                        bb = "map_extent")
  osm_paths <- osm_all_roads$osm_lines %>% 
    filter(highway %in% c("path","footway")) %>%
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf) %>% 
    filter(is.na(golf))
  
  golf_paths <- osm_all_roads$osm_lines %>% 
    filter(highway %in% c("path","footway")) %>%
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf) %>% 
    filter(!is.na(golf))
    
  tracks <- osm_all_roads$osm_lines %>% 
    filter(highway %in% c("track"))%>%
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf) 
  
  tertiary <- osm_all_roads$osm_lines %>% 
    filter(highway %in% c("tertiary")) %>%
    st_intersection(fcwa_outer) %>% 
    st_transform(ncrs)

  
  secondary <- osm_all_roads$osm_lines %>% 
    filter(highway %in% c("secondary")) %>%
    st_intersection(fcwa_outer) %>% 
    st_transform(ncrs)
  motorway <- osm_all_roads$osm_lines %>% 
    filter(highway %in% c("motorway","motorway_link","secondary")) %>%
    st_intersection(fcwa_outer) %>% 
    st_transform(ncrs)
  service <- osm_all_roads$osm_lines %>% 
    filter(highway %in% c("service")) %>% 
    mutate(length = as.numeric(st_length(.))) %>% 
    dplyr::select(length) %>% 
    filter(length > 200) %>%
    st_intersection(fcwa_outer) %>% 
    st_transform(ncrs)
  
  buildings <- dt_build$osm_polygons %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
    
  #Query railroad
  railway <- query_osm(key = "railway", bb = "map_extent")$osm_lines  %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
  
  #Natural features
  natural_features <- query_osm(key = "natural", bb = "map_extent")$osm_polygons  %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
  
  wood <- natural_features %>% filter(natural == "wood")
  wetland <- natural_features %>% filter(natural == "wetland")
  water <- natural_features %>% filter(natural == "water")
  
  #Power lines
  power_lines <- query_osm(key = "power", bb = "map_extent")$osm_lines  %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
  
  #Bridges
  bridges <- query_osm(key = "bridge", bb = "map_extent")$osm_lines  %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
  
  #Accessibility
  access <- query_osm(key = "access", bb = "map_extent")$osm_polygons  %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
  
  landuse <- query_osm(key = "landuse", bb = "map_extent")$osm_polygons %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
  
  base_area <- query_osm(key = "landuse", bb = "map_extent")$osm_multipolygons %>% 
    st_transform(ncrs) %>% 
    filter(name %in% c("Joint Base Cape Cod",
                       "Military Training Area Camp Edwards")) %>% 
    sf::st_make_valid() %>% 
    st_union()%>% 
    st_intersection(map_extent_sf) 
  
  ccourse <- query_osm(key = "leisure", bb = "map_extent")$osm_polygons  %>% 
    st_transform(ncrs) %>% 
    st_intersection(map_extent_sf)  
  
  course <- 
    landuse %>% 
    filter(landuse == "grass") %>%
    bind_rows(ccourse %>% 
                filter(leisure == "golf_course")) %>% 
    st_intersection(map_extent_sf)
  plot(course)
  greens <- 
    landuse %>% 
    filter(golf == "fairway") %>% 
    st_transform(ncrs)
  
  save(greens, course, ccourse, tracks, landuse, access, bridges, power_lines,
       wood, wetland, water,golf_paths,
       buildings,base_area,
       natural_features, railway, 
       osm_paths, osm_all_roads, file = here::here("falmouth/gis/falmouth_osm_data.rdata"))
} else {
  load(here::here("falmouth/gis/falmouth_osm_data.rdata"))
}

# road and railway processing----
np_large_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("motorway","motorway_link")) %>% 
  st_transform(ncrs) %>% 
  st_intersection(map_extent_sf) 

primary_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("primary","trunk")) %>% 
  st_transform(ncrs) %>% 
  st_intersection(map_extent_sf) 

tertiary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("unclassified","tertiary")) %>%
  st_transform(ncrs) %>% 
  st_intersection(map_extent_sf) %>% 
  st_cast("MULTILINESTRING") %>% 
  st_cast("LINESTRING")

rotary <- 
  osm_all_roads$osm_polygons %>% 
  filter(highway %in% c("tertiary","unclassified")) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(ncrs) #%>% 
  # st_geometry() %>% 
  # st_cast("LINESTRING")

ggplot() +
  geom_sf(data = rotary )

service <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("service")) %>% 
  st_transform(ncrs) %>% 
  st_intersection(map_extent_sf) %>% 
  filter(st_geometry_type(.) == "LINESTRING")

residential <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("residential"))%>% 
  st_transform(ncrs) %>% 
  st_intersection(map_extent_sf) %>% 
  st_cast("LINESTRING")

residential2 <- 
  osm_all_roads$osm_polygons %>% 
  filter(highway == "residential") %>% 
  st_cast("LINESTRING") %>% 
  st_transform(ncrs) 

secondary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("secondary")) %>% 
  st_transform(ncrs) %>% 
  st_intersection(map_extent_sf) %>% 
  filter(st_geometry_type(.) == "LINESTRING")

snet1 <- 
  sfnetworks::st_network_join(sfnetworks::as_sfnetwork(np_large_roads),
                              sfnetworks::as_sfnetwork(primary_roads)) %>% 
  sfnetworks::st_network_join(.,sfnetworks::as_sfnetwork(secondary)) %>% 
  st_as_sf("edges")  %>% 
  smoothr::densify(.,n = 11) %>% 
  dplyr::select(highway) %>%
  st_transform(ncrs) %>%
  st_buffer(dist = 5.5,
            endCapStyle = "SQUARE") %>%
  mutate(grp = "large")

snet2 <- 
  sfnetworks::st_network_join(sfnetworks::as_sfnetwork(tertiary),
                              sfnetworks::as_sfnetwork(rotary)) %>% 
  st_as_sf("edges")  %>% 
  smoothr::densify(.,n = 10) %>% 
  dplyr::select(highway) %>% 
  st_transform(ncrs) %>% 
  st_buffer(dist = 5) %>%
  mutate(grp = "medium")  

snet3 <- sfnetworks::as_sfnetwork(service) %>% 
  st_as_sf("edges")  %>% 
  smoothr::densify(.,n = 10) %>% 
  dplyr::select(highway) %>% 
  st_transform(ncrs) %>% 
  st_buffer(dist = 3) %>%
  mutate(grp = "small")  

snet4 <- sfnetworks::st_network_join(sfnetworks::as_sfnetwork(residential),
                                     sfnetworks::as_sfnetwork(residential2)) %>%
  st_as_sf("edges") %>% 
  dplyr::select(highway) %>% 
  smoothr::densify(.,n = 10) %>% 
  st_transform(ncrs) %>% 
  st_buffer(dist = 4) %>% 
  mutate(grp = "small2") %>% 
  bind_rows(.,snet1) %>%
  bind_rows(.,snet2) %>%
  bind_rows(.,snet3) %>% 
  # st_intersection() %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_transform(ncrs) %>% 
  st_intersection(map_extent_sf)

rw1 <- sfnetworks::as_sfnetwork(railway) %>% 
  st_as_sf("edges") %>% 
  dplyr::select(highway) %>% 
  smoothr::densify(.,n = 10) %>% 
  st_buffer(dist = 3.5) %>% 
  mutate(grp = "rail") %>% 
  st_union() %>% 
  st_as_sf()


#slope and aspect----
slope = terrain(dem, opt='slope')
aspect = terrain(dem, opt='aspect')
hill = hillShade(slope, aspect, 10, 270)
hill_df <- hill %>% 
  dream::rst_to_tib(var_name = "slope")
full_map <- st_sf(geom = st_sfc(st_point(c(map_extent[1],map_extent[3])),
                                st_point(c(map_extent[2], map_extent[4]))),
                  crs = 4326) %>% 
  st_transform(ncrs)

# contours-----
cont <- rasterToContour(dem, nlevels = 20)
cont_sf <- as(cont, "sf") %>% 
  smoothr::smooth(., method = "ksmooth", smoothness = 10) %>%
  st_cast("LINESTRING") %>% 
  mutate(level = as.numeric(level),
         length = as.numeric(st_length(.)),
         id = 1:nrow(.)) %>% 
  filter(length > 250) %>%
  st_crop(full_map)
 
t <- 
  sf::st_coordinates(cont_sf) %>% 
  as_tibble() %>% 
  left_join(.,cont_sf %>% 
              mutate(id = 1:nrow(.)) %>% 
              st_set_geometry(NULL),
            by = c("L1" = "id")) %>% 
  distinct() %>% 
  mutate(cont_lab = ifelse(level %in% seq(10, 70, 10),
                           "show","no show"))

# t2 <- NULL
# for (i in 1:length(unique(t$L1))){
#   df <- 
#     t %>% filter(L1 == unique(t$L1)[i]) %>% 
#     add_row(., slice_head(., n = 1))
#   assign("t2", rbind(df, t2))
# }


# make isobands----
m <- as.matrix(dem)

x <- coordinates(dem) %>% 
  as.data.frame() %>% 
  pull(x) %>% 
  unique

y <- coordinates(dem) %>% 
  as.data.frame() %>% 
  pull(y) %>% 
  unique

b <- isobands(x, 
              y, 
              m, 
              seq(-10, 70, 10), seq(0, 80, 10))
bands <- iso_to_sfg(b)

bands_sf <- 
  st_sf(
    level = 1:length(bands),
    geometry = st_sfc(bands,
                      crs = ncrs)
  ) %>% 
  smooth(., method = "ksmooth", smoothness = 10) %>%
  mutate(bin = factor(ntile(level, 7)))

#plot pre-sets
dt_inset_color <- "black"
trail_color <- "purple"
hs_alpha <- 0.5
mp <- 31.5
bg_fill <- "transparent"

out <-
  ggplot() +
  
  # hill shade
  geom_raster(data = hill_df, aes(x = longitude, y = latitude,
                                  fill = slope), show.legend = F) +
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill()+
  geom_sf(data = bands_sf, aes(fill = bin), color = "transparent",
          alpha = 0.65)+
  scale_fill_manual(values = colorRampPalette(c("#D9F0D3", "#5AAE61", "#00441B"))(7)) +

  # # dem
  # geom_raster(data = dem_df, aes(x = longitude, y = latitude,
  #                                      fill = fill_var),
  #             alpha = hs_alpha,
  #             show.legend = F) +
  # scale_fill_gradientn(colours = c("#D9F0D3", "#5AAE61", "#00441B")) +

  # geom_path(data = t %>%
  #               filter(cont_lab == "no show"),
  #             aes(x = X, y = Y, group = L1,
  #                 alpha = level),
  #           size = 0.1, padding  = unit(0, "pt"),
  #           color = "black") +
  # 
  # geom_textpath(data = t %>%
  #                 filter(cont_lab == "show"),
  #               aes(x = X, y = Y, group = L1, label = level,
  #                   alpha = level),
  #               size = 1, padding  = unit(0, "pt"),
  #               linewidth = 0.2, color = "black") +
  geom_sf(data = course %>% filter(golf %in% c("rough","grass", NA)),
          fill = "#9dcf9f80", color = "transparent") +
  geom_sf(data = course %>%
            filter(golf == "fairway"), 
          fill = "#58755980", color = "transparent") +
  
  geom_sf(data = osm_paths, color = "white", linewidth = 0.5) +
  geom_sf(data = osm_paths, color = "#9d39db", linewidth = 0.3) +
  geom_sf(data = osm_paths, color = "#6e5e44", linewidth = 0.2, lty = "11") +
  geom_sf(data = golf_paths, color = "grey30", linewidth = 0.3, lty = "11") +
  
  geom_sf(data = tracks, color = "white", linewidth = 0.5) +
  geom_sf(data = tracks, color = "grey30", linewidth = 0.3, linetype = "21" ) +
  
  #Natural features
  geom_sf(data = wetland, 
          color = "transparent", fill = "#b8936e", alpha = 0.25) +
  geom_sf(data = water,
          color = "transparent", fill = "#78A2CC") +
  geom_sf(data = rw1, color = "grey50", 
          fill = "#544d3d",
          linewidth = 0.1) +
  # roads
  geom_sf(data = snet4, linewidth = 0.1, color = "grey50",
          fill = 'grey85') +
  geom_sf(data = buildings, linewidth = 0.1, fill = "grey50") + 
  geom_sf(data = power_lines, color = "black", linewidth = 0.1) +
  geom_sf(data = base_area,
          color = "#b5473fb3",
          fill = "transparent") +

  #Theme----------------------------------------------------------------------
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  guides(alpha = "none",
         color= "none",
         fill = "none") +
  theme(rect = element_blank(),
        panel.grid= element_line(color = "grey30",
                                 linewidth = 0.2),
        panel.ontop = TRUE,
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  coord_sf(datum = st_crs(ncrs))

ggsave(out, 
       filename = file.path(pdf.dir,"FCWA_clipped_raw.svg"),
       width = 12, 
       height = 18.55,
       units = "in")
