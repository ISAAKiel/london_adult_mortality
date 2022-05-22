# Backbone of Europe data
path <- ("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/european_module/FinalData170713.xlsx")
# https://www.asc.ohio-state.edu/economics/osu/data/FinalData170713.xlsx
BoE <- readxl::read_xlsx(path = path, sheet = 1)
BoE_ext <- data.frame(BoE$SEQID, BoE$SITE, BoE$SITENAME, BoE$Country, BoE$TIME, BoE$BCENT, BoE$AGE, BoE$Recode_age_cat, BoE$SEX_CAT, BoE$LAT, BoE$LONG, BoE$REGION)
colnames(BoE_ext) <- c("id", "site_id", "site", "country", "period", "century", "age", "age_code", "sex", "lat", "long", "region")
BoE_ext <- subset(BoE_ext, age >= 15)
BoE_sites <- BoE_ext %>% group_by(site_id, site, period, lat, long, region) %>% summarize(n = n(), mean_century = median(century))
BoE_sites_subset <- subset(data.frame(BoE_sites), n > 49)
BoE_ext_subset <- BoE_ext[BoE_ext$site_id %in% BoE_sites_subset$site_id, ]
BoE_ext_subset$age_code <- substr(BoE_ext_subset$age_code,2,nchar(BoE_ext_subset$age_code)-1)
BoE_ext_subset <- BoE_ext_subset %>% tidyr::separate(age_code, c("agebeg", "ageend"), sep = ",") %>%
  transform(agebeg = as.numeric(agebeg), ageend = as.numeric(ageend) -1 )

min_latitude <- min(BoE_ext_subset$lat)
max_latitude <- max(BoE_ext_subset$lat)
min_longitude <- min(BoE_ext_subset$long)
max_longitude <- max(BoE_ext_subset$long)
coastline_50m <- sf::read_sf("/Users/Nils/Documents/Aktuelle_Dokumente/Weltkarte/50m_physical/ne_50m_coastline.shp")
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_coastline.zip
lakes_50m <- sf::read_sf("/Users/Nils/Documents/Aktuelle_Dokumente/Weltkarte/50m_physical/ne_50m_lakes.shp")
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip
rivers_lakes_50m <- sf::read_sf("/Users/Nils/Documents/Aktuelle_Dokumente/Weltkarte/50m_physical/ne_50m_rivers_lake_centerlines.shp")
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_rivers_lake_centerlines.zip
BoE_result_sf <- sf::st_as_sf(BoE_ext_subset, coords = c("long", "lat"), crs = 4326, agr = "constant")