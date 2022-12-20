# Libraries needed
library(osmplotr)
library(ggplot2)
library(sf)
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
bbox <- get_bbox(
  c(
    min(sites_data$lon) - (0.1*(max(sites_data$lon)- min(sites_data$lon))),
    min(sites_data$lat) - (0.1*(max(sites_data$lat)- min(sites_data$lat))),
    max(sites_data$lon) + (0.1*(max(sites_data$lon)- min(sites_data$lon))),
    max(sites_data$lat) + (0.1*(max(sites_data$lat)- min(sites_data$lat)))
  )
)

# Querry the osm data
dat_Adm8 <- extract_osm_objects (key = "boundary", value = "administrative", extra_pairs = c("admin_level", "8"), bbox = bbox)
#dat_Adm6 <- extract_osm_objects (key = "boundary", value = "administrative", extra_pairs = c("admin_level", "6") bbox = bbox)
#dat_Res <- extract_osm_objects (key = "landuse", value = "residential", geom_only = TRUE, bbox = bbox)
#dat_Hwy <- extract_osm_objects (key = "highway", value = "primary", geom_only = TRUE, bbox = bbox)
#dat_water <-extract_osm_objects (key = "natural", "water", bbox=bbox)
#dat_Wwy <-extract_osm_objects (key = "water", value="river", bbox=bbox)


# Build the map
London_map <- ggplot() +
  geom_sf(data = dat_Adm8, aes()) +
  geom_sf_text(data = dat_Adm8, aes(label=sub('.*of ','',dat_Adm8$name)), size=3) +
  geom_sf(data = dat_sites,aes(), shape = 16, colour = "black", size = 2) +
  ggrepel::geom_label_repel(data = dat_sites, aes(label = nr, geometry = geometry),
  stat = "sf_coordinates", min.segment.length = 0, size=4) +
  annotate("label", x = -0.06, y = 51.450, hjust = 0, size = 3,
           label = paste(apply(sites_data[,1:2],1,paste,collapse = ": "), 
                         collapse = "\n")) +
  xlim (-0.28, 0.03) +
  ylim (51.41,NA) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  theme(axis.title = element_blank())+
  ggspatial::annotation_scale(location = 'tl', height= unit(0.15, "cm"))
plot(London_map)

# Save the finished map object
ggsave(
  filename = "london_map.pdf",
  plot = London_map, 
  device = "pdf",
  path = "documented"
)

ggsave(
  filename = "london_map.png",
  plot = London_map, 
  device = "png",
  path = "documented"
)
