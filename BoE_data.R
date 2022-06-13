# Backbone of Europe data
BoE <-  rio::import("https://www.asc.ohio-state.edu/economics/osu/data/FinalData170713.xlsx")
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

BoE_table_ind <- table(BoE_ext_subset$period, BoE_ext_subset$region) %>%
  addmargins() %>%
  as.data.frame.matrix() %>%
  tibble::rownames_to_column("period") %>%
  knitr::kable(., caption = "Global History of health, breakdown of individuals by region and period.")

BoE_table_sites <- table(BoE_sites_subset$period, BoE_sites_subset$region) %>%
  addmargins() %>%
  as.data.frame.matrix() %>%
  tibble::rownames_to_column("period") %>%
  knitr::kable(., caption = "Global History of health, breakdown of sites by region and period.")