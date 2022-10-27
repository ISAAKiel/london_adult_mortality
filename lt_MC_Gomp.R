lt.MC.Gomp <- function(
  pop_inc = c(0.01), # vector of population increase
  pop_start = c(1000), # starting population
  beta = 0.05, # Gompertz beta
  years = 100, # duration of observation
  obs_start,
  obs_end,
  bayes = FALSE
) {
  start_time <- Sys.time()
  alpha <- exp(-66.77 * (beta - 0.0718) - 7.119)
  lt_result <- data.frame(pop_inc = NA, j = NA, 
                          death_count_n = NA, surv_Gompertz_shape = NA, 
                          surv_Gompertz_rate = NA,
                          bayes_gomp_b = NA,
                          bayes_gomp_a = NA,
                          bayes_gomp_M = NA,
                          bayes_gomp_r = NA,
                          bayes_gomp_unity = NA)
  pop_inc_length <- length(pop_inc)
  for (g in 1:pop_inc_length) {
    years_df <- data.frame()
    pop_actu <- pop_start[g]
    for (t in 1:years) {
      ind_df <- data.frame(t = t, ind = 1:pop_actu) %>%
        mutate(age = (round(flexsurv::rgompertz(n(), beta, alpha) ) + 15) ) %>% 
        mutate(death_t = t + age - 15) %>% 
        mutate(age_beg = ifelse(age < 18, 15,
                                ifelse(age < 26, 18,
                                       ifelse(age < 36, 26,
                                              ifelse(age < 46, 36, 46))))) %>% 
        mutate(age_end = ifelse(age < 18, 18,
                                ifelse(age < 26, 26,
                                       ifelse(age < 36, 36,
                                              ifelse(age < 46, 46, 100)))))
      
      years_df <- rbind(years_df, ind_df)
      pop_actu <- round(pop_actu * (1 + pop_inc[g]))
    }
    
    # sampling of death counts
    for (j in obs_start:obs_end) {
      death_count <- subset(years_df, death_t == j)
      death_count_n <- nrow(death_count)
      # fit Gompertz distribution to known age with Survival package + individual age
      death_count$death <- 1
      ind_dfGomp <- flexsurv::flexsurvreg(formula = survival::Surv(age - 15, death) ~ 1, data = death_count, dist="gompertz")
      surv_Gompertz_shape <- ind_dfGomp$coefficients[1]
      surv_Gompertz_rate <- exp(ind_dfGomp$coefficients[2])
      
      bayes_gomp_b <- NA
      bayes_gomp_a <- NA
      bayes_gomp_M <- NA
      bayes_gomp_r <- NA
      bayes_gomp_unity <- NA
      if (bayes == TRUE) {
      gomp.anthr_age.r(death_count, age_beg = "age_beg", age_end = "age_end",
                     silent.jags = TRUE,
                     silent.runjags = TRUE,
                     thinSteps = 1,
                     numSavedSteps = 10000,
                     minimum_age = 15,
                     r = pop_inc[g]) %>%
        diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
        bayes_gomp_b <- gomp_anthr_MCMC_diag[2,5]
        bayes_gomp_a <- gomp_anthr_MCMC_diag[1,5]
        bayes_gomp_M <- gomp_anthr_MCMC_diag[3,5]
        bayes_gomp_r <- gomp_anthr_MCMC_diag[4,5]
        bayes_gomp_unity <- gomp_anthr_MCMC_diag[5,5]
      }
      
      lt_result[nrow(lt_result) + 1,] <- c(pop_inc = pop_inc[g], j, 
                                           death_count_n, 
                                           surv_Gompertz_shape, 
                                           surv_Gompertz_rate,
                                           bayes_gomp_b,
                                           bayes_gomp_a,
                                           bayes_gomp_M,
                                           bayes_gomp_r,
                                           bayes_gomp_unity)
      }
  }
  
  lt_result <- lt_result[-1,]
  
  rownames(lt_result) <- NULL
  
  end_time <- Sys.time()
  print(end_time - start_time)
  return(lt_result)
}
