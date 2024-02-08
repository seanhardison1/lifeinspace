library(whitebox)
library(raster)
library(rgdal)
library(rnaturalearth)
library(terra)
library(sp)
library(sf)
library(tidyverse)
library(rmapshaper)

load(here::here("data/sbt_dem.rdata"))

ncrs <- st_crs(stb_dem)

hp_poly <- st_read(here::here("data/hp_poly.kml")) %>% 
  st_transform(st_crs(stb_dem))

## 1. FeaturePreservingSmoothing
wbt_feature_preserving_smoothing(
  dem = "./data/steamboat_dem2.tif",
  output = "./data/smoothed_dem.tif",
  filter = 9
)

## 2. BreachDepressions
wbt_breach_depressions(dem = "./data/smoothed_dem.tif", output = "./data/breached.tif")

## 3. DInfFlowAccumulation
wbt_d_inf_flow_accumulation(input = "./data/breached.tif", output = "./data/flow_accum.tif")

flows <- raster(here::here("data/flow_accum.tif")) %>% 
  crop(.,hp_poly) #%>% 
  # aggregate(.,1.5)

flows2 <- flows > 5
plot(flows2)
r1 <- drawLine() 
r2 <- drawLine()
r3 <- drawLine()
r4 <- drawLine()
r5 <- drawLine()
r6 <- drawLine()
r7 <- drawLine()
r8 <- drawLine()
r9 <- drawLine()

r1_sf <- as(r1, "sf")
r2_sf <- as(r2, "sf")
r3_sf <- as(r3, "sf")
r4_sf <- as(r4, "sf")
r5_sf <- as(r5, "sf")
r6_sf <- as(r6, "sf")
r7_sf <- as(r7, "sf")
r8_sf <- as(r8, "sf")
r9_sf <- as(r9, "sf")

creeks <- r1_sf %>% 
  bind_rows(.,r2_sf) %>% 
  bind_rows(.,r3_sf) %>% 
  bind_rows(.,r4_sf) %>% 
  bind_rows(.,r5_sf) %>% 
  bind_rows(.,r6_sf) %>% 
  bind_rows(.,r7_sf) %>% 
  bind_rows(.,r8_sf) %>% 
  bind_rows(.,r9_sf) %>% 
  st_set_crs(.,ncrs) %>% 
  smoothr::smooth(.,method = "ksmooth",
                  smoothness = 1)

plot(creeks)

save(creeks, file = here::here("data/creeks.rdata"))
