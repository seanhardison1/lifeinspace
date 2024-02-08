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
library(metR)
library(geomtextpath)
library(ggisoband)
library(nord)
library(ggpattern)


ncrs <- "+proj=utm +zone=13 +datum=NAD83 +units=km +no_defs"

load(here::here("data/sbt_dem.rdata"))
stb_dem <- projectRaster(stb_dem, crs = ncrs)

stb_dem <- stb_dem / 1000

load(here::here("data/creeks.rdata"))
creeks <- creeks %>% st_transform(.,crs = ncrs)

hpfs <- c("hp2017.kml",
          "hp2018.kml",
          "hp2019.kml",
          "hp2020.kml",
          "hp2022.kml")

hogan_tracks <- NULL
for (i in hpfs){
  init <- st_read(here::here("data",i)) %>% 
    st_cast("LINESTRING") %>% 
    st_transform(ncrs) %>% 
    mutate(year = factor(str_extract(i, "\\d{4}")))
  
  assign("hogan_tracks", rbind(init, hogan_tracks))
}

hp_poly <- st_read(here::here("data/hp_poly.kml")) %>% 
  st_transform(ncrs)


ggplot() +
  geom_sf(data = hogan_tracks %>% filter(!year %in% c(2022, 2020)),
          aes(color = year))

hp_poly_sub <- st_read(here::here("data/hp_poly2.kml")) %>% 
  st_transform(ncrs)

source(here::here("R/query_osm.R"))

hp_dem <- stb_dem %>% 
  crop(.,hp_poly_sub)#%>%  
  # raster::aggregate(fact = 5)
  # dream::rst_to_tib() %>%
  # as.data.frame()

# create slope and hillshade
slope = terrain(hp_dem, opt='slope')
aspect = terrain(hp_dem, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill_df <- hill %>% 
  dream::rst_to_tib(var_name = "slope")

# get topography
# plot(aspect)

# full dem
hp_dem_df <- hp_dem %>% 
  dream::rst_to_tib() #%>% 
  # mutate(bin = factor(ntile(fill_var, 5)))

# plot(hp_dem)
# query OSM----
process <- F
if (process){
  hp_poly2 <- st_read(here::here("data/hp_poly.kml"))# %>% 
    # st_transform(ncrs)
  osm_all_roads <- query_osm(key = "highway", 
                             bb = "hp_poly2")
  water <- query_osm(key = "water",
                     bb = "hp_poly2")
  liftlines <- 
    query_osm(key = "aerialway",
              bb = "hp_poly2")

  lines <- query_osm(key = "power", bb = "hp_poly2")
  natural <- query_osm(key = "natural", bb = "hp_poly2")
  natural2 <- query_osm(key = "landuse", bb = "hp_poly2")
  bridges <- query_osm(key = "bridge", select = "osm_lines",bb = "hp_poly2")
  access <- query_osm(key = "access", select = "osm_polygons",bb = "hp_poly2")
  waterway <- query_osm(key = "waterway",
                        bb = "hp_poly2")
  dt_build <- query_osm(key = "building",
                        bb = "hp_poly2")

  away <- query_osm(key = "aerialway",
                    bb = "hp_poly2")
  rodeo <- query_osm(key = "sport",
                     bb = "hp_poly2")
  baseball <- query_osm(key = "leisure",
                        bb = "hp_poly2")
  place <- query_osm(key = "place",
                     bb = "hp_poly2")
  
  fp <- opq(bbox = st_bbox(hp_poly2)) %>%
    add_osm_feature(key = "highway", value = NULL) %>%
    osmdata_sf()
  
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

primary_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("primary","trunk")) %>% 
  st_transform(ncrs)

tertiary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("unclassified","tertiary")) %>%
  st_transform(ncrs)

service <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("service","track")) %>%
  st_transform(ncrs)

trails <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("path",
                        "cycleway",
                        "track")) %>%
  st_transform(ncrs)

ms_lift <- liftlines$osm_lines %>% 
  filter(name == "Morningside Lift")%>%
  st_transform(ncrs)

ms_lift_points <- ms_lift %>% 
  st_cast("POINT")%>%
  st_transform(ncrs)

hw40 <- st_intersection(osm_all_roads$osm_lines %>% 
                  filter(ref == "US 40") %>% 
                  st_transform(ncrs),
                hp_poly) %>%
  # smooth(., method = "ksmooth", smoothness = 1) %>% 
  st_union() %>%
  st_transform(ncrs)
  # as_sfnetwork() %>%
  # st_buffer(dist = 10)

resort <- natural2$osm_polygons %>% 
  filter(name == "Steamboat Resort") %>% 
  st_transform(ncrs) %>% 
  st_crop(.,hp_poly)

# pal <- c("#D3D6D1",
#        # "white",
#        "#9DACB7"
#   "#9DACB7",
#   "#D3D6D1",
#   "white"
#        "#98A8B4"
#        "blue"
#        )

max(hp_dem_df$fill_var)

# steeps 
cont1 <- seq(2300/1000, 2800/1000, 55/1000)
length(cont1)
# 
# pal1 <- c("#6E87A0","#B0BCC4")
# 
# p1 <- colorRampPalette(pal1)(length(cont1))

# shallows
step <- 45/1000
cont2_min <- max(cont1) + step
cont2 <- seq(cont2_min, 3270/1000, step)
length(cont2)

# pal2 <- c("#CED4D5",
# "#EEF0F0",
# "#E5E8E9"
# "#C6CED0",
# "#CED4D5",
# "#FFFFFF")

# p2 <- colorRampPalette(pal2)(length(cont2))

brks <- c(cont1, cont2)
n <- length(brks)
# combine
# p <- c(p1, p2)
# pal <- c("#4f7092","white")
# p <- colorRampPalette(pal)(length(brks))

# p[9] <- "red"

# [1] "#4F7092" "#577797" "#607E9C" "#6985A2" "#728CA7" "#7B93AD" "#839AB2" "#8CA2B8" "#95A9BD" "#9EB0C3" "#A7B7C8"
# [12] "#AFBECD" "#B8C5D3" "#C1CCD8" "#CAD4DE" "#D3DBE3" "#DBE2E9" "#E4E9EE" "#EDF0F4" "#F6F7F9" "#FFFFFF"
# 
# smacpod::gradient.color.scale(minval = min(cont1),
#                               maxval = max(cont2),
#                               midpoint = n
#                               low = "#4F7092",
                              # mid = )

# ggplot(hp_dem_df) + 
#   geom_histogram(aes(x = fill_var, fill = ..x..),
#                  bins = n,
#                  breaks = brks) +
#   scale_fill_stepsn(colors = p,
#                     n.breaks = n,
#                     breaks = brks)
# 
# [1] "#6E87A0" "#758CA4" "#7C92A8" "#8498AC" "#8B9EB0" "#92A4B4" "#9AAAB8" "#A1B0BC" "#A9B6C0" "#B0BCC4" "#B7C2C8"
# [12] "#BFC8CC" "#C6CED0" "#CED4D5" "#D5DADB" "#DDE1E2" "#E5E8E9" "#EEF0F0" "#F6F7F7" "#FFFFFF"
# 
# [1] "#6E87A0" "#7990A6" "#8499AD" "#8FA2B3" "#9AABBA" "#A5B3C1" "#B0BDC7" "#BBC6CE" "#C6CFD4" "#D1D8DB" "#DDE1E2"
# [12] "#E5E8E9" "#E8EAEB" "#EBEDEE" "#EEF0F1" "#F2F3F3" "#F5F6F6" "#F8F9F9" "#FBFCFC" "#FFFFFF"
# 
# [1] "#6E87A0" "#7A91A7" "#869BAE" "#93A5B6" "#9FAFBD" "#ABB9C4" "#B8C3CB" "#C4CCD3" "#D0D6DA" "#DDE1E2" "#CED4D5"
# [12] "#D5DADB" "#DCE0E1" "#E3E6E7" "#EAECED" "#F1F2F3" "#F8F8F9" "#FFFFFF"
# 
# 
# 
# 
# 
# 
# 
# 
# et <- c(Earth_Tone_1 = '#B8B17F', Earth_Tone_2 = '#B3AD7A', Earth_Tone_3 = '#AEA976', 
#   arth_Tone_4 = '#A9A572', Earth_Tone_5 = '#A4A06D', Earth_Tone_6 = '#9F9C68', 
#   Earth_Tone_7 = '#9A9864', Earth_Tone_8 = '#95945F', Earth_Tone_9 = '#908F5B', 
#   arth_Tone_10 = '#8B8B56', Earth_Tone_11 = '#868751', Earth_Tone_12 = '#81834D',
#   Earth_Tone_13 = '#7C7E48', Earth_Tone_14 = '#777A44', Earth_Tone_15 = '#72763F', 
#   Earth_Tone_16 = '#6D723B', Earth_Tone_17 = '#686E36', Earth_Tone_18 = '#636A32', 
#   Earth_Tone_19 = '#5E662D', Earth_Tone_20 = '#595228')
# 
# 
# 
# ws <- rev(c(Winter_Sunrise_1 = '#FADADD', Winter_Sunrise_2 = '#F5D8DC', 
#   Winter_Sunrise_3 = '#F0D7DB', Winter_Sunrise_4 = '#EBD5DA',
#   Winter_Sunrise_5 = '#E6D3D9', Winter_Sunrise_6 = '#E1D2D8', 
#   Winter_Sunrise_7 = '#DCD1D7', Winter_Sunrise_8 = '#D7CFD6', 
#   Winter_Sunrise_9 = '#D2CED5', Winter_Sunrise_10 = '#CDCCD4', 
#   Winter_Sunrise_11 = '#C8CBD3', Winter_Sunrise_12 = '#C3C9D2', 
#   Winter_Sunrise_13 = '#BEC8D1', Winter_Sunrise_14 = '#B9C6D0', 
#   Winter_Sunrise_15 = '#B4C5CF', Winter_Sunrise_16 = '#AFC3CE', 
#   Winter_Sunrise_17 = '#AAC2CD', Winter_Sunrise_18 = '#A5C0CC', 
#   Winter_Sunrise_19 = '#A0BFCB', Winter_Sunrise_20 = '#9BBECA'))
# 
# 
# c(Winter_Sunrise_Canyon_Emphasis_1 = '#3e5587', 
#   Winter_Sunrise_Canyon_Emphasis_2 = '#3f5587', 
#   Winter_Sunrise_Canyon_Emphasis_3 = '#3f5687', 
#   Winter_Sunrise_Canyon_Emphasis_4 = '#daa49a', W
#   inter_Sunrise_Canyon_Emphasis_5 = '#daa49a', 
#   Winter_Sunrise_Canyon_Emphasis_6 = '#daa59b', 
#   Winter_Sunrise_Canyon_Emphasis_7 = '#daa59b', 
#   Winter_Sunrise_Canyon_Emphasis_8 = '#dba59b', 
#   Winter_Sunrise_Canyon_Emphasis_9 = '#dba69c', 
#   Winter_Sunrise_Canyon_Emphasis_10 = '#dba69c', 
#   Winter_Sunrise_Canyon_Emphasis_11 = '#dba69c',
#   Winter_Sunrise_Canyon_Emphasis_12 = '#dba79d', 
#   Winter_Sunrise_Canyon_Emphasis_13 = '#dba79d', 
#   Winter_Sunrise_Canyon_Emphasis_14 = '#dba79d', 
#   Winter_Sunrise_Canyon_Emphasis_15 = '#dca79d', 
#   Winter_Sunrise_Canyon_Emphasis_16 = '#dca89e', 
#   Winter_Sunrise_Canyon_Emphasis_17 = '#dca89e', 
#   Winter_Sunrise_Canyon_Emphasis_18 = '#dca89e', 
#   Winter_Sunrise_Canyon_Emphasis_19 = '#dca99f', Winter_Sunrise_Canyon_Emphasis_20 = '#dca99f')
# 
# 
# colorRampPalette(c('#B9C6D0',"#6E87A0"))(7)
# colorRampPalette(c('#EBD5DA',"snow"))(4)
# 
# 
# "#EBD5DA" "#F1E1E4" "#F8EDEF" "#FFFAFA"
# 
# ws <- rev(c(Winter_Sunrise_1 = "#FFFAFA", Winter_Sunrise_2 = "#F8EDEF", 
#             Winter_Sunrise_3 = "#F1E1E4", Winter_Sunrise_4 = '#EBD5DA',
#             Winter_Sunrise_5 = '#E6D3D9', Winter_Sunrise_6 = '#E1D2D8', 
#             Winter_Sunrise_7 = '#DCD1D7', Winter_Sunrise_8 = '#D7CFD6', 
#             Winter_Sunrise_9 = '#D2CED5', Winter_Sunrise_10 = '#CDCCD4', 
#             Winter_Sunrise_11 = '#C8CBD3', Winter_Sunrise_12 = '#C3C9D2', 
#             Winter_Sunrise_13 = '#BEC8D1', Winter_Sunrise_14 = "#B9C6D0",
#             "#ACBBC8",
#             "#A0B1C0",
#             "#93A6B8",
#             "#879CB0",
#             "#7A91A8",
#             "#6E87A0"))
# 
# 
# af <- c(Alpine_Flora_1 = '#6C5971', Alpine_Flora_2 = '#69586C', 
#   Alpine_Flora_3 = '#665666', Alpine_Flora_4 = '#635561', 
#   Alpine_Flora_5 = '#60545C', Alpine_Flora_6 = '#5D5356', 
#   Alpine_Flora_7 = '#5A5251', Alpine_Flora_8 = '#57514B', 
#   Alpine_Flora_9 = '#545046', Alpine_Flora_10 = '#514F41', 
#   Alpine_Flora_11 = '#4E4E3B', Alpine_Flora_12 = '#4B4D36', 
#   Alpine_Flora_13 = '#484C31', Alpine_Flora_14 = '#454B2B', 
#   Alpine_Flora_15 = '#424A26', Alpine_Flora_16 = '#3F4921', 
#   Alpine_Flora_17 = '#3C481C', Alpine_Flora_18 = '#394716', 
#   Alpine_Flora_19 = '#364611', Alpine_Flora_20 = '#33450C')
# 
# Alpine_Flora_9 = '#545046', Alpine_Flora_10 = '#514F41', 
# Alpine_Flora_11 = '#4E4E3B', Alpine_Flora_12 = '#4B4D36', 
# Alpine_Flora_13 = '#484C31', Alpine_Flora_14 = '#454B2B', 
# Alpine_Flora_15 = '#424A26', Alpine_Flora_16 = '#3F4921', 
# Alpine_Flora_17 = '#3C481C', Alpine_Flora_18 = '#394716', 
# Alpine_Flora_19 = '#364611', Alpine_Flora_20 = '#33450C'
# 
# '#33450C', '#FADADD'
#   
# ss <- 
#   c(Sunset_in_Snow_1 = '#C0B9DD', Sunset_in_Snow_2 = '#BCB6DB', 
#   Sunset_in_Snow_3 = '#B8B2D9', Sunset_in_Snow_4 = '#B4AFD7', 
#   Sunset_in_Snow_5 = '#B0ACD5', Sunset_in_Snow_6 = '#ACA9D3', 
#   Sunset_in_Snow_7 = '#A8A6D1', Sunset_in_Snow_8 = '#A4A3CF', 
#   Sunset_in_Snow_9 = '#A0A0CD', Sunset_in_Snow_10 = '#9C9DCB', 
#   Sunset_in_Snow_11 = '#989AC9', Sunset_in_Snow_12 = '#9497C7', 
#   Sunset_in_Snow_13 = '#9094C5', Sunset_in_Snow_14 = '#8C91C3', 
#   Sunset_in_Snow_15 = '#888EC1', Sunset_in_Snow_16 = '#848BBF', 
#   Sunset_in_Snow_17 = '#8088BD', Sunset_in_Snow_18 = '#7C85BB', 
#   Sunset_in_Snow_19 = '#7882B9', Sunset_in_Snow_20 = '#747FB7')
# 

p1 <- c('#3e5587',
        '#C8CBD3')
p2 <- c('#CDCCD4',
        '#F0BBB0')

p1 <- c('#3e5587',
        "#EEF0F1")
p2 <- c("#F2F3F3",
        '#F0BBB0')

p1 <- c('#3e5587',
        "#EEF0F1")
p2 <- c("#F2F3F3",
        '#E5B3A9')

ws <- c(colorRampPalette(p1)(13),
        colorRampPalette(p2)(7))

crk_pal <- "#2E5F8FCC"

track_pal <- c('#DC143CCC', 
               '#4169E1CC', 
               '#50C878CC', 
               '#FFD700CC', 
               '#800080CC')

track_pal <- c('#17A2B8', 
                '#FF6B6B', 
                # '#32CD32', 
               '#50C878CC',
                '#7D3C98', 
                '#DAA520')


out <-

ggplot() +
    geom_raster(data = hill_df,
                aes(x = longitude, y = latitude, fill = slope),
                show.legend = F) +
    scale_fill_gradient(low = "black", high = "white") +
    new_scale_fill() +
    geom_isobands(data = hp_dem_df %>% filter(fill_var > 2.3), 
                  aes(x = longitude, 
                                        y = latitude, 
                                        z = fill_var,
                                        fill = fill_var),
                  bins = n,
                  alpha = 0.8,
                  size = 0.05,
                  breaks = brks,
                  show.legend = F) +
    scale_fill_stepsn(colors = ws,
                      breaks = brks) +
  geom_sf_pattern(data = resort,
                  pattern = 'stripe',
                  pattern_color = "black",
                  pattern_alpha = 0.1,
                  pattern_angle = 120,
                  fill = NA,
                  pattern_density = 0.001) +
  geom_sf(data = creeks, color = crk_pal) +
  geom_sf(data = hogan_tracks, aes(color = year),
          linewidth = 1, show.legend = F) +
  scale_color_manual(values = track_pal) +
  geom_sf(data = ms_lift) +
  geom_sf(data = hw40, linewidth = 1.75, color = "grey60")  +
  geom_sf(data = hw40, linewidth = 1.25, color = "grey80")  +
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  theme(rect = element_blank(),
        panel.grid= element_line(color = "grey60",
                                 linewidth = 0.15),
        panel.ontop = TRUE,
        axis.ticks = element_blank(),
        axis.title = element_blank())+
        # axis.text = element_blank()) +
  coord_sf(xlim = c(351500/1000,358000/1000),
           ylim = c( 4471500/1000, 4480000/1000),
           crs = ncrs,
           datum = ncrs)
out


gs <- c(Dark_Slate_Gray='#4F4F4F', Dim_Gray='#5A5A5A', 
       Medium_Gray='#656565', Granite_Gray='#707070', 
       Gray_Cloud='#7B7B7B', Silver_Chalice='#868686', 
       Roman_Silver='#919191', Quick_Silver='#9C9C9C', 
       Spanish_Gray='#A7A7A7', Light_Gray='#B2B2B2',
       Gainsboro='#BDBDBD', Light_Silver='#C8C8C8', 
       Platinum='#D3D3D3', Silver_Sand='#DEDEDE', 
       Silver='#E9E9E9', Cultured='#F4F4F4', White_Smoke='#FCFCFC', 
       Ghost_White='#FDFDFD', Snow='#FEFEFE', Pure_White='#FFFFFF')

out_gs <-
  ggplot() +
  geom_raster(data = hill_df,
              aes(x = longitude, y = latitude, fill = slope),
              show.legend = F) +
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill() +
  geom_isobands(data = hp_dem_df, aes(x = longitude, 
                                      y = latitude, 
                                      z = fill_var,
                                      fill = fill_var),
                bins = n,
                alpha = 0.8,
                size = 0.05,
                breaks = brks,
                show.legend = F) +
  scale_fill_stepsn(colors = gs,
                    breaks = brks) +
  geom_sf_pattern(data = resort,
                  pattern = 'stripe',
                  pattern_color = "black",
                  pattern_alpha = 0.1,
                  pattern_angle = 120,
                  fill = NA,
                  pattern_density = 0.001) +
  geom_sf(data = creeks, color = crk_pal) +
  geom_sf(data = hogan_tracks, aes(color = year),
          linewidth = 1, show.legend = F) +
  scale_color_manual(values = track_pal) +
  geom_sf(data = ms_lift) +
  geom_sf(data = hw40, linewidth = 1.75, color = "grey60")  +
  geom_sf(data = hw40, linewidth = 1.25, color = "grey80")  +
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  theme(rect = element_blank(),
        panel.grid= element_line(color = "grey60",
                                 linewidth = 0.15),
        panel.ontop = TRUE,
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  coord_sf(xlim = c(351500/1000,358000/1000),
           ylim = c( 4471500/1000, 4480000/1000),
           crs = ncrs,
           datum = ncrs)


ggsave(out_gs, 
       filename = here::here("figs/test4_greyscale.svg"),
       width = 16, 
       height = 25,
       units = "in")

ggsave(out, 
       filename = here::here("figs/test4_color.svg"),
       width = 16, 
       height = 25,
       units = "in")



ggplot() +
  geom_isobands(data = hp_dem_df %>% filter(fill_var > 2.3 & fill_var <3), 
                aes(x = longitude, 
                    y = latitude, 
                    z = fill_var,
                    fill = fill_var),
                bins = n,
                alpha = 0.8,
                size = 0.05,
                breaks = brks) +
  scale_fill_stepsn(colors = ws,
                    breaks = brks) +
  theme(legend.position = "bottom")
