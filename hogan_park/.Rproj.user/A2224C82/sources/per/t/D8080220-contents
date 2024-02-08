library(raster)
library(sf)
library(tidyverse)

plot(raster::raster(here::here("data/steamboat_dem.tif")))

d <- raster::raster(here::here("data/steamboat_dem.tif"))
stb <- sf::st_read(here::here("data/steamboat.kml")) %>% 
  sf::st_zm() %>% 
  st_transform(.,crs = st_crs(d)) %>% 
  fasterize::fasterize(.,d)

#steamboat
crop_extent <- extent(c(xmin = -107.1,
                        xmax = -106.5,
                        ymin = 40.3,
                        ymax = 40.7))
stb_dem <- crop(d, crop_extent)
plot(stb_dem)
save(stb_dem, file = here::here("data/sbt_dem.rdata"))
load(here::here("data/sbt_dem.rdata"))
plot(stb_dem)
