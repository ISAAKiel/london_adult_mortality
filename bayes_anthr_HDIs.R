sampling = 100
n_min = 50
n_max = 500
M_min = NULL
M_max = NULL
b_min = 0.02
b_max = 0.1
error_range = 15
age_categories = "Wellcome" # either Museum of London Wellcome database or of the Backbone of Europe project


start_time <- Sys.time()
lt_result <- data.frame()
for (g in 1:sampling) {
  # sampling with Gompertz distribution
  y <- round(runif(n = 1, min = n_min, max = n_max))
  if(is.null(M_min) | is.null(M_max)) {
    b_ <- runif(n = 1, min = b_min, max = b_max)
    a_ <- exp(rnorm(1, (-66.77 * (b_ - 0.0718) - 7.119), sqrt(0.0823) ) )
  } else {
    M <- round(runif(n = 1, min = M_min, max = M_max))
    M_1 <- 0
    M_2 <- 0
    while ( (M < M_1 | M > M_2 )) {
      b_ <- runif(n = 1, min = b_min, max = b_max)
      a_ <- exp(rnorm(1, (-66.77 * (b_ - 0.0718) - 7.119), sqrt(0.0823) ) )
      M_ <- 1 / b_ * log (b_/a_) + 15
      M_1 <- M_ - 1
      M_2 <- M_ + 1
    }
  }
  
  ind_df <- data.frame(ind = NA, age = NA, age_beg = NA, age_end = NA)
  for (i in 1:y) {
    x <- round(flexsurv::rgompertz(1, b_, a_) ) + 15
    if(length(error_range) > 0) {
      x_used <- round(rnorm(1, x, error_range))
    } else {
      x_used <- x
    }
    if(age_categories == "Wellcome") {
      if(x_used < 18) {
        age_beg = 15
        age_end = 18
      } else if(x_used < 26) {
        age_beg = 18
        age_end = 26
      } else if(x_used < 36) {
        age_beg = 26
        age_end = 36
      } else if(x_used < 46) {
        age_beg = 36
        age_end = 46
      } else {
        age_beg = 46
        age_end = 100
      } 
    } else {
      if(x_used < 20) {
        age_beg = 15
        age_end = 19
      } else if(x_used < 25) {
        age_beg = 20
        age_end = 24
      } else if(x_used < 30) {
        age_beg = 25
        age_end = 29
      } else if(x_used < 35) {
        age_beg = 30
        age_end = 34
      } else if(x_used < 40) {
        age_beg = 35
        age_end = 39
      } else {
        age_beg = 40
        age_end = 99
      } 
    }
    
    ind_df <- rbind(ind_df, c(i, x, age_beg, age_end))
  }
  ind_df <- ind_df[-1,]
  
  gomp.anthr_age(ind_df, age_beg = "age_beg", age_end = "age_end",
                 silent.jags = TRUE,
                 silent.runjags = TRUE,
                 thinSteps = 1,
                 numSavedSteps = 10000,
                 minimum_age = 15) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  bayes_anthr_gomp_b <- gomp_anthr_MCMC_diag[2,5]
  bayes_anthr_gomp_a <- gomp_anthr_MCMC_diag[1,5]
  bayes_anthr_max_PSRF <- max(gomp_anthr_MCMC_diag[,1])
  bayes_anthr_max_PSRF_upper <- max(gomp_anthr_MCMC_diag[,2])
  bayes_anthr_min_ESS <- min(gomp_anthr_MCMC_diag[,6])
  bayes_anthr_gomp_b_HDIlow <- gomp_anthr_MCMC_diag[2,9]
  bayes_anthr_gomp_b_HDIhigh <- gomp_anthr_MCMC_diag[2,10]
  ind_result <- cbind(y, a_, b_, 
                      bayes_anthr_gomp_b, bayes_anthr_gomp_a,
                      bayes_anthr_max_PSRF, bayes_anthr_max_PSRF_upper,
                      bayes_anthr_min_ESS, bayes_anthr_gomp_b_HDIlow,
                      bayes_anthr_gomp_b_HDIhigh)
  lt_result <- rbind(lt_result, ind_result)
  
  svMisc::progress(g/sampling * 100, (sampling-1)/sampling * 100, progress.bar = TRUE)
  Sys.sleep(0.0001)
  if (g == sampling) message("Done!")
}
rownames(lt_result) <- NULL

end_time <- Sys.time()
print(end_time - start_time)

min_ESS <- min(lt_result$bayes_anthr_min_ESS)
max_PSRF_upper <- max(lt_result$bayes_anthr_max_PSRF_upper)
max_PSRF <- max(lt_result$bayes_anthr_max_PSRF)
mean_bias <- mean(lt_result$b_ - lt_result$bayes_anthr_gomp_b)
mean_inaccuracy <- mean(abs(lt_result$b_ - lt_result$bayes_anthr_gomp_b))
rmse <- Metrics::rmse(lt_result$b_, lt_result$bayes_anthr_gomp_b)
sd_bias <- sd(lt_result$b_ - lt_result$bayes_anthr_gomp_b)
mean_HDI_range <- mean(lt_result$bayes_anthr_gomp_b_HDIhigh - lt_result$bayes_anthr_gomp_b_HDIlow)
anthr_result <- data.frame(min_ESS, max_PSRF, max_PSRF_upper, mean_bias, mean_inaccuracy, rmse, sd_bias)

lt_result$accuracy <- ifelse(lt_result$bayes_anthr_gomp_b_HDIhigh < lt_result$b_, "underest.",
                             ifelse(lt_result$bayes_anthr_gomp_b_HDIlow > lt_result$b_, "overest.", "correct"))
lt_result$HDI_range <- lt_result$bayes_anthr_gomp_b_HDIhigh - lt_result$bayes_anthr_gomp_b_HDIlow

ggplot(lt_result) + geom_point(aes(x = b_, y = HDI_range, group=accuracy, colour=accuracy))
ggplot(lt_result) + geom_point(aes(x = y, y = HDI_range, group=accuracy, colour=accuracy))

lt_result %>% group_by(y_ = ceiling(y/100) * 100, accuracy) %>%
  tally() %>% spread(accuracy, n)
lt_result %>% group_by(y_ = ceiling(HDI_range*200) / 200, accuracy) %>%
  tally() %>% spread(accuracy, n)
lt_result %>% group_by(beta = ceiling(b_*50) / 50, accuracy) %>%
  tally() %>% spread(accuracy, n)
