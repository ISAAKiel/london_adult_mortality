sampling = 100
n_min = 50
n_max = 500
M_min = NULL
M_max = NULL
b_min = 0.02
b_max = 0.1
error_range = 15


start_time <- Sys.time()
lt_result <- data.frame()
for (g in 1:sampling) {
  ind_df <- lt.sampling(1, n_min = 50, n_max = 500, b_min = 0.02, b_max = 0.1)
  
  gomp.anthr_age(ind_df, age_beg = "age_beg", age_end = "age_end",
                 silent.jags = TRUE,
                 silent.runjags = TRUE,
                 thinSteps = 1,
                 numSavedSteps = 100000,
                 minimum_age = 15) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  bayes_anthr_gomp_b <- gomp_anthr_MCMC_diag[2,5]
  bayes_anthr_gomp_a <- gomp_anthr_MCMC_diag[1,5]
  bayes_anthr_max_PSRF <- max(gomp_anthr_MCMC_diag[,1])
  bayes_anthr_max_PSRF_upper <- max(gomp_anthr_MCMC_diag[,2])
  bayes_anthr_min_ESS <- min(gomp_anthr_MCMC_diag[,6])
  bayes_anthr_gomp_b_HDIlow <- gomp_anthr_MCMC_diag[2,9]
  bayes_anthr_gomp_b_HDIhigh <- gomp_anthr_MCMC_diag[2,10]
  ind_result <- cbind(y = ind_df[1,2], b_ = ind_df[1,3], a_ = ind_df[1,4], 
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
