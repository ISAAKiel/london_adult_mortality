# get the coordinates of sites to be plotted
library(osmplotr)
library(ggplot2)
sites_data <- rbind.data.frame(
    c("1","St. Marylebone", 51.5225,-0.152222),
    c("2","St. Mary Graces",51.509289,-0.072916 ),
    c("3","Merton Priory",51.414517,-0.181272),
    c("4","St. Benet",51.513194,-0.091389),
    c("5","St. Bride",51.513889,-0.100278),
    c("6","Bermondesy Abbey", 51.4975,-0.080833)
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

# querry the osm data
dat_Adm8 <- extract_osm_objects (key = "boundary", value = "administrative", extra_pairs = c("admin_level", "8"), bbox = bbox)
#dat_Adm6 <- extract_osm_objects (key = "boundary", value = "administrative", extra_pairs = c("admin_level", "6") bbox = bbox)
#dat_Res <- extract_osm_objects (key = "landuse", value = "residential", geom_only = TRUE, bbox = bbox)
#dat_Hwy <- extract_osm_objects (key = "highway", value = "primary", geom_only = TRUE, bbox = bbox)
#dat_water <-extract_osm_objects (key = "natural", "water", bbox=bbox)
#dat_Wwy <-extract_osm_objects (key = "water", value="river", bbox=bbox)


#build the map
ggplot() +
  geom_sf(data = dat_Adm8, aes()) +
  geom_sf(data = dat_sites,aes(), shape = 16, colour = "black", size = 2) +
  geom_sf_label(data = dat_sites, aes(label = nr, vjust=-.25), size=4) +
  geom_sf_text(data =dat_Adm8, aes(label=sub('.*of ','',dat_Adm8$name)), size=3)+
  xlim (-0.28, 0.05) +
  ylim (51.38,NA) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  theme(axis.title = element_blank()) 
  