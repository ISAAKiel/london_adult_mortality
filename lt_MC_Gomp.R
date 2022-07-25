lt.MC.Gomp <- function(
  pop_inc = c(0.01), # vector of population increase
  pop_actu = 1000, # starting population
  beta = 0.05, # Gompertz beta
  years = 100, # duration of observation
  obs_start,
  obs_end
) {
  start_time <- Sys.time()
  alpha <- exp(-66.77 * (beta - 0.0718) - 7.119)
  lt_result <- data.frame(pop_inc = NA, j = NA, death_count_n = NA, surv_Gompertz_shape = NA, surv_Gompertz_rate = NA)
  pop_inc_length <- length(pop_inc)
  for (g in 1:pop_inc_length) {
    years_df <- data.frame()
    for (t in 1:years) {
      ind_df <- data.frame(t = t, ind = 1:pop_actu) %>%
        mutate(age = (round(flexsurv::rgompertz(n(), beta, alpha) ) + 15) ) %>% mutate(death_t = t + age - 15)
      
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
      
      lt_result[nrow(lt_result) + 1,] <- c(pop_inc = pop_inc[g], j, death_count_n, surv_Gompertz_shape, surv_Gompertz_rate)
    }
  }
  
  lt_result <- lt_result[-1,]
  
  rownames(lt_result) <- NULL
  
  end_time <- Sys.time()
  print(end_time - start_time)
  return(lt_result)
}
