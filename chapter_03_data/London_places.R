# Get the coordinates of sites to be plotted
sites_data <- rbind.data.frame(
    c("1","St. Marylebone", 51.5225,-0.152222),
    c("2",paste("St. Marylebone's Paddington", "Street north", sep="\n"), 51.520869, -0.154515),
    c("3","St. Mary Graces",51.509289,-0.072916 ),
    c("4","St. Benet Sherehog",51.513194,-0.091389),
    c("5","St. Bride's crypt",51.513802,-0.105292),
    c("6","Bermondsey Abbey", 51.4975,-0.080833),
    c("7", "New Churchyard", 51.517403, -0.084216),
    c("8", "St. Mary Spital", 51.518716, -0.079161),
    c("9", "Chelsea Old Church", 51.483222, -0.170795),
    c("10", "St. Bride's lower churchyard", 51.515253, -0.104973),
    c("11", "St. Mary and St. Michael", 51.51330, -0.05190),
    c("12", "Sheen's burial ground", 51.51480, -0.06760),
    c("13", "Bow Baptist church", 51.529540, -0.01580)
  ) 
colnames(sites_data) <-c("nr", "name", "lat", "lon")
sites_data$lat<-as.numeric(sites_data$lat)
sites_data$lon<-as.numeric(sites_data$lon)

dat_sites <- st_as_sf(sites_data, 
                      coords = c("lon", "lat"), 
                      crs = 4326)

# Build a bounding box by the coordinates + 10% of the extent as frame
bbox <- matrix(
  c(
    min(sites_data$lon) - (0.1*(max(sites_data$lon)- min(sites_data$lon))),
    min(sites_data$lat) - (0.1*(max(sites_data$lat)- min(sites_data$lat))),
    max(sites_data$lon) + (0.1*(max(sites_data$lon)- min(sites_data$lon))),
    max(sites_data$lat) + (0.1*(max(sites_data$lat)- min(sites_data$lat)))
  ), 
  byrow = FALSE, nrow = 2, ncol = 2,
  dimnames = list(c('x','y'),c('min','max'))
)

# Querry the osm data
# If the bbox has been changed please run the statement to get the new osm data.

if (!exists ("q_admin8") | (runCodeNew == TRUE)) {
  q_admin8 <- bbox %>% opq() %>% 
    add_osm_feature(key = "boundary", value = "administrative") %>%
    add_osm_feature(key = "admin_level", value = "8") %>%
    osmdata_sf()
  }    

# Build the map
London_map <- ggplot() +
  geom_sf(data = q_admin8$osm_multipolygons, fill=rgb(0.9,0.9,0.9)) +
  geom_sf_text(data = q_admin8$osm_multipolygons, aes(label=sub('.*of ','',name)), size=3) +
  geom_sf(data = dat_sites,aes(), shape = 16, colour = "black", size = 2) +
  ggrepel::geom_label_repel(data = dat_sites, aes(label = nr, geometry = geometry),
  stat = "sf_coordinates", min.segment.length = 0, size=4) +
  annotate("label", x = -0.28, y = 51.38, hjust = 0, vjust = 0, size = 3,
                    label = paste(apply(sites_data[1:6,1:2],1,paste,collapse = ": "), 
                         collapse = "\n")) +
  annotate("label", x = -0.12, y = 51.38, hjust = 0, vjust = 0, size = 3,
           label = paste(apply(sites_data[7:13,1:2],1,paste,collapse = ": "), 
                         collapse = "\n")) +
  xlim (-0.28, 0.03) +
  ylim (51.38,51.58) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  theme(axis.title = element_blank()) +
  ggspatial::annotation_scale(location = 'tl', height= unit(0.15, "cm"))
plot(London_map)

# Save the finished map object
ggsave(
  filename = "fig04_london_map.pdf",
  plot = London_map, 
  device = "pdf",
  path = "documented"
)