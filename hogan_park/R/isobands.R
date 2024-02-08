
#map constants
dt_inset_color <- "black"
trail_color <- "purple"
hs_alpha <- 0.5
mp <- 180
n_bands <- 20

# make isobands
m <- as.matrix(hp_dem) * 3.28084

x <- coordinates(hp_dem) %>% 
  as.data.frame() %>% 
  pull(x) %>% 
  unique

y <- coordinates(hp_dem) %>% 
  as.data.frame() %>% 
  pull(y) %>% 
  unique

min_elev <- signif(min(m), 1)
max_elev <- signif(max(m), 3)
step <- 300

b <- isobands(x, 
              y, 
              m, 
              seq(min_elev, max_elev - step, length.out = n_bands),
              seq(min_elev + step, max_elev, length.out = n_bands))
bands <- iso_to_sfg(b)

hp_sf <- 
  st_sf(
    level = 1:length(bands),
    elev = seq(min_elev, (max_elev - step), 
               length.out = n_bands),
    geometry = st_sfc(bands,
                      crs = ncrs)
  ) %>%
  # smooth(., method = "ksmooth", smoothness = 10) %>% 
  st_transform(ncrs) %>%
  mutate(level_grp = factor(elev))


t <-
  sf::st_coordinates(hp_sf) %>%
  as_tibble() %>%
  left_join(.,hp_sf %>%
              mutate(id = 1:nrow(.)) %>%
              st_set_geometry(NULL),
            by = c("L3" = "id")) %>%
  distinct() %>%
  mutate(grp = paste(L3, L2, L1))

