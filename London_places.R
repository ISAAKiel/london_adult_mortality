# still unfinished, for plotting sites in London
london_data <- rbind.data.frame(
    c("St. Marylebone", 51.5225,-0.152222),
    c("St. Mary Graces",51.509289,-0.072916 ),
    c("Merton Priory",51.414517,-0.181272),
    c("St. Benet",51.513194,-0.091389),
    c("St. Bride",51.513889,-0.100278),
    c("Bermondesy Abbey", 51.4975,-0.080833)
  ) 
colnames(london_data) <-c("name", "lat", "lon")
london_data$lat<-as.numeric(london_data$lat)
london_data$lon<-as.numeric(london_data$lon)

dat_sites <- st_as_sf(london_data, 
                      coords = c("lon", "lat"), 
                      crs = 4326)

bbox <- get_bbox(
  c(
    min(london_data$lon) - 0.01,
    min(london_data$lat) - 0.01,
    max(london_data$lon) + 0.01,
    max(london_data$lat) + 0.01
  )
)

dat_Res <- extract_osm_objects (key = "landuse", value = "residential", geom_only = TRUE, bbox = bbox)
dat_Hwy <- extract_osm_objects (key = "highway", value = "primary", geom_only = TRUE, bbox = bbox)
#dat_Adm <- extract_osm_objects (key = "boundary", value = "administrative", extra_pairs = c("admin_level", "8"), bbox = bbox)

map <- osm_basemap (bbox = bbox, bg = "gray90")
#map <- add_osm_objects (map, dat_Adm, col = "gray70")
map <- add_osm_objects (map, dat_Res, col = "gray60")
map <- add_osm_objects (map, dat_Hwy, col = "gray20")
map <- add_osm_objects(map, dat_sites, col = "black", size=2)

print_osm_map(map) 

#Baustelle

ggplot(data=map) 

+
  geom_sf(colour = "grey") +
  geom_sf(data = dat_sites, shape = 16, colour = "red", size = 5) +
  geom_sf_text(data = dat_sites, aes(label = name), nudge_x = c(5.5, 1, 4),
               nudge_y = c(0.75, -1, -0.75)) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  theme(axis.title = element_blank()) 

+
  coord_sf(bbox)

world <- ne_countries(scale = "medium", returnclass = "sf")
map_inset <- ggplot() +
  #geom_sf(colour = "grey") +
  geom_sf(data = tauber_utm, shape = 16, colour = "red", size = 4) +
  #  geom_sf_text(data = tauber_utm, aes(label = name), nudge_x = c(20000, 20000),
  #               nudge_y = c(-15000, -15000), size = 3) +
  geom_sf_text(data = tauber_utm, aes(label = name),
               nudge_x = c(-9000, 5000, 5500, 6500, 11500, -5000, -5000, -2000, 5000, 7000),
               nudge_y = c(2500, -2500, 2500, -2500, 0, -2500, 2500, 2500, 2500, 2500), size = 3) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  coord_sf(xlim =  c(530000, 570000), ylim = c(5470000, 5515000), expand = FALSE) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks =  element_blank()) +
  theme(axis.text = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,-1,-1), "mm")) +
  ggspatial::annotation_scale(location = 'bl', height= unit(0.15, "cm"))
cowplot::ggdraw() +
  draw_plot(map_main) +
  draw_plot(map_inset, x = 0.65, y = 0.5, width = 0.3, height = 0.4)
