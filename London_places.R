# still unfinished, for plotting sites in London

london_names <- c("St. Marylebone", "St. Mary Graces", "Merton Priory", "St. Benet", "St. Bride", "Bermondesy Abbey")
latitude <- c(51.5225, 51.509289, 51.414517, 51.513194, 51.513889, 51.4975)
longitude <- c(-0.152222, -0.072916, -0.181272, -0.091389, -0.100278, -0.080833)
london_data <- data.frame(london_names, latitude, longitude)
london_boundary_lat_max <- max(london_data$latitude) + 0.1
london_boundary_lat_min <- min(london_data$latitude) - 0.1
london_boundary_long_max <- max(london_data$longitude) + 0.1
london_boundary_long_min <- min(london_data$longitude) - 0.1
bbox <- get_bbox (c (london_boundary_long_min, london_boundary_lat_min, 
                     london_boundary_long_max, london_boundary_lat_max))
dat_B <- extract_osm_objects (key = "building", bbox = bbox)
map <- osm_basemap (bbox = bbox, bg = "gray20")
map <- add_osm_objects (map, dat_B, col = "gray40")
