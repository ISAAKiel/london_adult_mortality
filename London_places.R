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

bbox <- get_bbox(
  c(
    min(london_data$lon) + 0.1,
    min(london_data$lat) + 0.1,
    max(london_data$lon) - 0.1,
    max(london_data$lat) - 0.1
  )
)

dat_Res <- extract_osm_objects (key = "landuse", value = "residential", geom_only = TRUE, bbox = bbox)

map <- osm_basemap (bbox = bbox, bg = "gray20")
map <- add_osm_objects (map, dat_Res, col = "gray40")
plot(map)

