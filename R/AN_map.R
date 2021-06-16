
# Geocoding locations
# library(ggmap)
# 
# dat <- ggmap::mutate_geocode(dat, location = region_sampled)

# World map

library(sf)
library(tmap)
data("World")

sf_dat <- dat %>% 
  filter(!is.na(lon), !is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mm <- tm_shape(World) +
  tm_layout(bg.color = "grey95") +
  #tm_graticules(col = "white") +
  tm_polygons("grey50", lwd = 0.5, border.col = "grey80") +
  tm_shape(sf_dat) +
  tm_symbols(col = "black", border.col = "white", border.lwd = 0.8, size = 0.2)
#tm_scale_bar(breaks = c(0, 1000, 2000), position = c("RIGHT", "BOTTOM"))

mm

#mm %>% tmap_leaflet()





str_split(dat$trad_sampling_method, pattern = ";") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_squish() %>% 
  enframe(value = "method") %>% 
  count(method) %>% 
  arrange(desc(n)) %>%
  mutate(method_simple = case_when(n < 2 ~ "Others",
                                   n >= 2 ~ method)) %>% 
  group_by(method_simple) %>% 
  summarise(n = sum(n)) %>% 
  mutate(method_simple = fct_reorder(method_simple, n)) %>% 
           #fct_relevel("Others", after = 0)) %>%
  ggplot() +
  geom_col(aes(method_simple, n)) +
  coord_flip()
  
