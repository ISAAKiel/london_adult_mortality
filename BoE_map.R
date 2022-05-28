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
BoE_sites_subset$period <- factor(BoE_sites_subset$period, levels = c("Pre-medieval", "Early medieval", "High medieval", "Late medieval",  "Early modern", "Industrial") )

min_latitude <- min(BoE_ext_subset$lat)
max_latitude <- max(BoE_ext_subset$lat)
min_longitude <- min(BoE_ext_subset$long)
max_longitude <- max(BoE_ext_subset$long)
lakes_50m <- sf::st_as_sf(rnaturalearth::ne_download(scale = 50, type = 'lakes', category = 'physical'))
coastline_50m <- sf::st_as_sf(rnaturalearth::ne_download(scale = 50, type = 'coastline', category = 'physical'))
rivers_lakes_50m <- sf::st_as_sf(rnaturalearth::ne_download(scale = 50, type = 'rivers_lake_centerlines', category = 'physical'))
BoE_result_sf <- sf::st_as_sf(BoE_ext_subset, coords = c("long", "lat"), crs = 4326, agr = "constant")