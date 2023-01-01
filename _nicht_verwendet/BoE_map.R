min_latitude <- min(BoE_ext_subset$lat)
max_latitude <- max(BoE_ext_subset$lat)
min_longitude <- min(BoE_ext_subset$long)
max_longitude <- max(BoE_ext_subset$long)
lakes_50m <- sf::st_as_sf(rnaturalearth::ne_download(scale = 50, type = 'lakes', category = 'physical'))
coastline_50m <- sf::st_as_sf(rnaturalearth::ne_download(scale = 50, type = 'coastline', category = 'physical'))
rivers_lakes_50m <- sf::st_as_sf(rnaturalearth::ne_download(scale = 50, type = 'rivers_lake_centerlines', category = 'physical'))
BoE_result_sf <- sf::st_as_sf(BoE_ext_subset, coords = c("long", "lat"), crs = 4326, agr = "constant")
BoE_map <- ggplot() +
  geom_sf(data = rivers_lakes_50m, colour = "light blue", size = 0.3) +
  geom_sf(data = lakes_50m, colour = "light blue", fill = "light blue", size = 0.2) +
  geom_sf(data = coastline_50m, colour = "dark grey", size = 0.2) +
  geom_sf(data = BoE_result_sf, shape = 21, size = 2, aes(colour = region)) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  coord_sf(ylim =  c(-1 + min_latitude, max_latitude + 1), xlim = c(-2 + min_longitude, max_longitude + 1), expand = FALSE)