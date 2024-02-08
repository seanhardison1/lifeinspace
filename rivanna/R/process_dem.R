library(tidyverse)
library(raster)
library(sf)

rt_extent <- st_read(here::here("data/rt_poly.kml")) %>% 
  st_transform(crs = "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs") %>% 
  st_zm()

rt_dem <- raster(here::here("data/USGS_one_meter_x71y422_VA_ChesapeakeBaySouth_2015.tif")) %>% 
  crop(.,rt_extent)%>% 
  raster::aggregate(.,fact = 2.5)

rt_dem_full <- raster(here::here("data/USGS_one_meter_x71y422_VA_ChesapeakeBaySouth_2015.tif")) %>% 
  crop(.,rt_extent)

save(rt_dem, rt_extent, rt_dem_full,
     file = here::here("data/rt_dem_processed.rdata"))
