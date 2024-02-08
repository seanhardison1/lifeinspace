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

load(here::here("data/rt_dem_processed.rdata"))
source(here::here("R/query_osm.R"))
ncrs <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"


# create slope and hillshade
slope = terrain(rt_dem_full, opt='slope')
aspect = terrain(rt_dem_full, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill_df <- hill %>% 
  dream::rst_to_tib(var_name = "slope")

# get topography
rt_cont <- rasterToContour(rt_dem, nlevels = 25)

rt_cont_sf <- as(rt_cont, "sf") %>% 
  st_transform(ncrs) %>% 
  smoothr::smooth(., method = "ksmooth", smoothness = 10) %>%
  st_cast("LINESTRING") %>% 
  mutate(level = as.numeric(level),
         length = as.numeric(st_length(.)),
         id = 1:nrow(.)) %>% 
  filter(length > 100 | (length > 100 & level > 45))

t <- 
  sf::st_coordinates(rt_cont_sf) %>% 
  as_tibble() %>% 
  left_join(.,rt_cont_sf %>% 
              mutate(id = 1:nrow(.)) %>% 
              st_set_geometry(NULL),
            by = c("L1" = "id")) %>% 
  distinct() %>% 
  mutate(cont_lab = ifelse(level %in% seq(140, 270, 20),
                           "show","no show"))

rt_cont2 <- dream::rst_to_tib(rt_dem)

# make isobands
# m <- as.matrix(rt_dem)
# 
# x <- coordinates(rt_dem) %>% 
#   as.data.frame() %>% 
#   pull(x) %>% 
#   unique
# 
# y <- coordinates(rt_dem) %>% 
#   as.data.frame() %>% 
#   pull(y) %>% 
#   unique
# 
# b <- isobands(x, 
#               y, 
#               m, 
#               10*(13:26), 10*(14:27))
# bands <- iso_to_sfg(b)
# 
# rt_sf <- 
#   st_sf(
#     level = 1:length(bands),
#     geometry = st_sfc(bands,
#                       crs = ncrs)
#   ) %>% 
#   smooth(., method = "ksmooth", smoothness = 4) %>%
#   # st_transform(4326) %>% 
#   mutate(bin = factor(ntile(level, 5)))
# 
# plot(rt_sf)
rt_dem_df <- rt_dem %>% 
  dream::rst_to_tib() %>% 
  mutate(bin = factor(ntile(fill_var, 5)))

process <- F
if (process){
  osm_all_roads <- query_osm(key = "highway", loc = "charlottesville va")
  lines <- query_osm(key = "power", loc = "charlottesville va")
  natural <- query_osm(key = "natural", loc = "charlottesville va")
  natural2 <- query_osm(key = "landuse", loc = "charlottesville va")
  bridges <- query_osm(key = "bridge", select = "osm_lines",loc = "charlottesville va")
  access <- query_osm(key = "access", select = "osm_polygons",loc = "charlottesville va")
  waterway <- query_osm(key = "waterway",
                        loc = "charlottesville va")
  dt_build <- query_osm(key = "building",
                        loc = "charlottesville va")
  water <- query_osm(key = "water",
                     loc = "charlottesville va")
  away <- query_osm(key = "aerialway",
                    loc = "charlottesville va")
  rodeo <- query_osm(key = "sport",
                     loc = "charlottesville va")
  baseball <- query_osm(key = "leisure",
                        loc = "charlottesville va")
  place <- query_osm(key = "place",
                     loc = "charlottesville va")
  
  parking <-
    opq ("charlottesville va") %>%
    add_osm_feature (key = "amenity", value = "parking") %>%
    osmdata_sf()
  
  save(osm_all_roads,
       lines,
       baseball,
       natural,
       natural2,
       bridges,
       dt_build,
       parking,
       access,
       waterway,
       water,
       away,
       rodeo,
       file = here::here("data/rt_osm_data.rdata"))
} else {
  load(here::here("data/rt_osm_data.rdata"))
}