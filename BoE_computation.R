BoE_result <- data.frame()
for (i in 1:length(unique(BoE_ext_subset$site_id)) ) {
  site_id_i <- unique(BoE_ext_subset$site_id)[i]
  site_data <- BoE_ext_subset[ which(BoE_ext_subset$site_id == site_id_i), ]
  site <- unique(site_data$site)
  period <- unique(site_data$period)
  site_data$age <- round(site_data$age)
  site_data$age2 <- round(site_data$age)
  
  mort_prep <- prep.life.table(site_data, agebeg = "agebeg", ageend = "ageend", agerange = "include", method = c(1, 4, 5, 5, 5, 5, 5, 5, 5, 60))
  mort_life <- life.table(mort_prep)
  x_length <- length(mort_life$x)
  x <- cumsum(mort_life$a)
  x_vec <- x[4:(x_length - 1)]
  x_vec2 <- c(x_vec[-1], Inf)
  Dx_vec <- mort_life$Dx[5:x_length]
  qx_vec <- mort_life$qx[5:x_length]
  x_mid <- ( x_vec + c(x_vec[-1], 99) ) / 2
  mort_df <- data.frame(x_vec,  x_vec2, x_mid, Dx_vec)
  
  #fit WOLS to life table qx which has to by divided by 1,000
  mort_fit_WOLS_estim <- lm(log(qx_vec*0.001)  ~ x_mid, data = mort_df, weights  = Dx_vec)
  WOLS_estim_Gompertz_shape <- mort_fit_WOLS_estim$coefficients[2]
  WOLS_estim_Gompertz_rate <- exp(mort_fit_WOLS_estim$coefficients[1])
  
  # fit Gompertz distribution to estimated age with Survival package + individual age
  site_data$death <- 1
  ind_dfGomp <- flexsurv::flexsurvreg(formula = survival::Surv(age - 15, death) ~ 1, data = site_data, dist="gompertz")
  surv_Gompertz_shape <- ind_dfGomp$coefficients[1]
  surv_Gompertz_rate <- exp(ind_dfGomp$coefficients[2])
  
  #Bayesian modell with anthropological age estimate
  gomp.anthr_age(site_data, age_beg = "agebeg", age_end = "ageend",
                 silent.jags = TRUE,
                 silent.runjags = TRUE) %>%
    diagnostic.summary(., HDImass = 0.95) -> BoE_anthr_MCMC_diag
  bayes_anthr_gomp_b <- BoE_anthr_MCMC_diag[2,5]
  bayes_anthr_gomp_a <- BoE_anthr_MCMC_diag[1,5]
  
  ind_result <- cbind(site_id = site_id_i, site, period,
                      WOLS_estim_Gompertz_shape, WOLS_estim_Gompertz_rate,
                      surv_Gompertz_shape,surv_Gompertz_rate,
                      bayes_anthr_gomp_b, bayes_anthr_gomp_a)
  BoE_result <- rbind(BoE_result, ind_result )
}
rownames(BoE_result) <- NULL
cols.num <- c("WOLS_estim_Gompertz_shape", "WOLS_estim_Gompertz_rate",
              "surv_Gompertz_shape", "surv_Gompertz_rate", "bayes_anthr_gomp_b", "bayes_anthr_gomp_a")
BoE_result[cols.num] <- sapply(BoE_result[cols.num],as.numeric)
BoE_result <- merge(BoE_result, BoE_sites_subset)
BoE_result$period <- factor(BoE_result$period, levels = c("Pre-medieval", "Early medieval", "High medieval", "Late medieval",  "Early modern", "Industrial") )


# BoE_result[order((BoE_result$period) ), ]
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

# regions <- BoE_result$region
# plot_list <- list()
# for (j in regions) {
#   region_data <- BoE_result[ which(BoE_result$region == j), ]
#   plot_list[[j]] <- ggplot(region_data, aes(x = mean_century, y = WOLS_estim_Gompertz_shape)) + geom_point() + ylab("Gompertz \u03B2") +
#     geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) + 
#     xlab(j) + xlim(200, 1900)
# }
# do.call(gridExtra::grid.arrange, plot_list)
