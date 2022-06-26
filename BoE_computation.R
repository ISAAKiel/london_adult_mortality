if (runCodeNew){
  set.seed(4510)
  start_time <- Sys.time()
  
  BoE_result <- data.frame()
  BoE_count <- length(unique(BoE_ext_subset$site_id))
  for (i in 1:BoE_count ) {
    
    site_id_i <- unique(BoE_ext_subset$site_id)[i]
    site_data <- BoE_ext_subset[ which(BoE_ext_subset$site_id == site_id_i), ]
    site <- unique(site_data$site)
    period <- unique(site_data$period)
    
    site_data <- site_data %>% mutate(age_beg_w = ifelse(age < 24, 15, age - 9))
    site_data <- site_data %>% mutate(age_end_w = ifelse(age > 40, 99, age + 10))
    
    # site_data <- site_data %>% mutate (age_beg_w = 
    #   ifelse(age < 18, 15,
    #   ifelse(age < 26, 18,
    #   ifelse(age < 36, 26,
    #   ifelse(age < 46, 36, 46
    #   ) ) ) ) )
    # site_data <- site_data %>% mutate (age_end_w = 
    #                         ifelse(age < 18, 17,
    #                                ifelse(age < 26, 25,
    #                                       ifelse(age < 36, 35,
    #                                              ifelse(age < 46, 45, 99
    #                                              ) ) ) ) )
    
    #Bayesian modell with anthropological age estimate
    bayes_anthr_gomp_b <- NA
    bayes_anthr_gomp_a <- NA
    tryCatch({#gomp.anthr_age(site_data, age_beg = "age_beg_w", age_end = "age_end_w",
      gomp.anthr_age(site_data, age_beg = "agebeg", age_end = "ageend",
                     silent.jags = TRUE,
                     silent.runjags = TRUE,
                     thinSteps = 20,
                     numSavedSteps = 20000) %>%
        diagnostic.summary(., HDImass = 0.95) -> BoE_anthr_MCMC_diag
      bayes_anthr_gomp_b <- BoE_anthr_MCMC_diag[2,5]
      bayes_anthr_gomp_a <- BoE_anthr_MCMC_diag[1,5]
      min_ESS <- min(BoE_anthr_MCMC_diag[,6])
      max_PSRF <- max(BoE_anthr_MCMC_diag[,1])
      max_PSRF_CI <- max(BoE_anthr_MCMC_diag[,2])
    }, error=function(e){})
    
    ind_result <- cbind(site_id = site_id_i, site, period,
                        bayes_anthr_gomp_b, bayes_anthr_gomp_a,
                        min_ESS, max_PSRF, max_PSRF_CI)
    BoE_result <- rbind(BoE_result, ind_result )
    
    svMisc::progress(i/BoE_count * 100, (BoE_count-1)/BoE_count * 100, progress.bar = TRUE)
    Sys.sleep(0.0001)
    if (i == BoE_count) message("Done!")
    
  }
  rownames(BoE_result) <- NULL
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
  cols.num <- c("bayes_anthr_gomp_b", "bayes_anthr_gomp_a", "min_ESS", "max_PSRF", "max_PSRF_CI")
  BoE_result[cols.num] <- sapply(BoE_result[cols.num],as.numeric)
  BoE_result <- merge(BoE_sites_subset, BoE_result)
  BoE_result$period <- factor(BoE_result$period, levels = c("Pre-medieval", "Early medieval", "High medieval", "Late medieval",  "Early modern", "Industrial") )
  colnames(BoE_result) <- c("id", "site", "period", "Lat", "Lon", "region", "n", "century", "Gompertz beta", "Gompertz alpha", "min. ESS", "max. PSRF", "max. PSRF CI")
  
  # saves results in Rda-object
  save(BoE_result, file = file.path(".", saveFileDir, "BoE_result.Rda") )
}
load(file.path(".", saveFileDir, "BoE_result.Rda") )

plot_all <- ggplot(subset(BoE_result, `min. ESS` > 5000 & `max. PSRF` < 1.1), 
                        aes(x = century, y = `Gompertz beta`) ) + 
  geom_point(aes(group = region, colour = region)) +
  geom_smooth(method='loess', span = 0.5, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) + 
  ylab("Gompertz \u03B2") + xlab("years AD") + xlim(200,1900) + ylim(0.01, 0.06)

plot_list <- list()
regions <- unique(BoE_result$region[ which(BoE_result$region != "Mediterranean")])
for (j in regions) {
  region_data <- subset(BoE_result, `min. ESS` > 5000 & `max. PSRF` < 1.1)[ which(BoE_result$region == j), ]
  plot_list[[j]] <- ggplot(region_data, aes(x = century, y = `Gompertz beta`)) + geom_point() + ylab("Gompertz \u03B2") +
    geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    xlab(j) + xlim(200, 1900) + ylim(0.01, 0.06)
}

BoE_result <- merge(BoE_sites_subset, BoE_result)

plot_urbanity <- list()
urbanity <- unique(BoE_result$urban)
for (j in urbanity) {
  urban_data <- subset(BoE_result, `min. ESS` > 5000 & `max. PSRF` < 1.1)[ which(BoE_result$urban == j), ]
  plot_urbanity[[j]] <- ggplot(urban_data, aes(x = century, y = `Gompertz beta`)) + geom_point() + ylab("Gompertz \u03B2") +
    geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    xlab(j) + xlim(200, 1900) + ylim(0.01, 0.06)
}
do.call(gridExtra::grid.arrange, c(plot_urbanity, ncol = 2))

BoE_good <- subset(BoE_result, `min. ESS` > 5000 & `max. PSRF` < 1.1)[,-c(9:11)]
BoE_bad <- subset(BoE_result, `min. ESS` < 5000 | `max. PSRF` > 1.1 )[,-c(9:11)]

plot_list_bad <- list()
for (j in BoE_bad$id) {
  site_data <- BoE_ext_subset[ which(BoE_ext_subset$site_id == j), ]
  site_name <- unique(site_data$site)
  plot_list_bad[[site_name]] <- ggplot(site_data, aes(x = agebeg)) + geom_histogram() + ggtitle(j)
}

set.seed(2930)
BoE_good_sample <- sample(BoE_good$id, 9)
plot_list_good <- list()
for (j in BoE_good_sample) {
  site_data <- BoE_ext_subset[ which(BoE_ext_subset$site_id == j), ]
  site_name <- unique(site_data$site)
  plot_list_good[[site_name]] <- ggplot(site_data) + geom_histogram(aes(x = agebeg)) + ggtitle(j)
}

# 
# BoE_result$M <- 1 / BoE_result$WOLS_estim_Gompertz_shape * 
#   log(BoE_result$WOLS_estim_Gompertz_shape/BoE_result$WOLS_estim_Gompertz_rate) + 15
# 
# ggplot(BoE_result, aes(x = WOLS_estim_Gompertz_shape, y = surv_Gompertz_shape ) ) + geom_point(aes(colour = period))
# 
# ggplot(BoE_result, aes(x = WOLS_estim_Gompertz_shape ) ) + geom_histogram(binwidth = 0.005)
# ggplot(BoE_result, aes(x = bayes_anthr_gomp_b ) ) + geom_histogram(binwidth = 0.005)
# 
# ggplot(BoE_result, aes(x = period, y = WOLS_estim_Gompertz_shape ) ) + geom_boxplot(aes(colour = period))
# ggplot(BoE_result, aes(x = period, y = bayes_anthr_gomp_b ) ) + geom_boxplot(aes(colour = period))
# ggplot(subset(BoE_result, M > 40), aes(x = period, y = M ) ) + geom_boxplot(aes(colour = period)) 
# ggplot(BoE_result, aes(x = region, y = WOLS_estim_Gompertz_shape ) ) + geom_boxplot()
# ggplot(BoE_result, aes(x = region, y = bayes_anthr_gomp_b ) ) + geom_boxplot()
# ggplot(BoE_result, aes(x = as.numeric(period), y = WOLS_estim_Gompertz_shape ) ) + geom_point() +
#   geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) + ylab("Gompertz \u03B2")
# ggplot(subset(BoE_result, bayes_anthr_gomp_b > 0.01), aes(x = as.numeric(period), y = bayes_anthr_gomp_b ) ) + geom_point() +
#   geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) + ylab("Gompertz \u03B2")
# ggplot(BoE_result, aes(x = mean_century, y = WOLS_estim_Gompertz_shape) ) + geom_point(aes(group = region, colour = region)) +
#   geom_smooth(method='loess', span = 0.5, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) + 
#   ylab("Gompertz \u03B2") + xlab("years AD")