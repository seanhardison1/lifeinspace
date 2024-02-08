library(tidyverse)
library(raster)
library(sf)

ohill_extent <- st_read(here::here("data/ohill_extent.kml")) %>% 
  st_transform(crs = "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs") %>% 
  st_zm()


ohill_extent2 <- st_read(here::here("data/ohill_outer.kml")) %>% 
  st_transform(crs = "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs") %>% 
  st_zm()

ohill_dem <- raster(here::here("data/USGS_one_meter_x71y422_VA_ChesapeakeBaySouth_2015.tif")) %>% 
  crop(.,ohill_extent2)%>% 
  raster::aggregate(.,fact = 2.5)

ohill_dem_full <- raster(here::here("data/USGS_one_meter_x71y422_VA_ChesapeakeBaySouth_2015.tif")) %>% 
  crop(.,ohill_extent2)

save(ohill_dem, ohill_extent, ohill_extent2,ohill_dem_full,
     file = here::here("data/ohill_dem_processed.rdata"))
