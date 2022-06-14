n_min = 50
n_max = 500
M_min = NULL
M_max = NULL
b_min = 0.025
b_max = 0.1
error_range = 15

y <- round(runif(n = 1, min = n_min, max = n_max))
b_ <- runif(n = 1, min = b_min, max = b_max)
a_ <- exp(rnorm(1, (-66.77 * (b_ - 0.0718) - 7.119), sqrt(0.0823) ) )

ind_df <- data.frame(ind = NA, age = NA, age_beg = NA, age_end = NA)
for (i in 1:y) {
  x <- round(flexsurv::rgompertz(1, b_, a_) ) + 15
  if(length(error_range) > 0) {
    x_used <- round(rnorm(1, x, error_range))
  } else {
    x_used <- x
  }
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
  
  ind_df <- rbind(ind_df, c(i, x, age_beg, age_end))
}
ind_df <- ind_df[-1,]

# Bayes model

gomp.known_age(ind_df, known_age = "age",
               thinSteps = 1,
               numSavedSteps = 10000) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_known_age_MCMC_diag_thinSteps1

gomp.known_age(ind_df, known_age = "age",
               thinSteps = 20,
               numSavedSteps = 100000) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_known_age_MCMC_diag_thinSteps20

gomp.anthr_age(ind_df, age_beg = "age_beg", age_end = "age_end",
               thinSteps = 1,
               numSavedSteps = 10000) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag_thinSteps1
gomp.anthr_age(ind_df, age_beg = "age_beg", age_end = "age_end",
               thinSteps = 20,
               numSavedSteps = 100000) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag_thinSteps20

rbind(cbind(mode = "known_age", thinning = 1, steps = 10000, parameter = row.names(gomp_known_age_MCMC_diag_thinSteps1), gomp_known_age_MCMC_diag_thinSteps1),
      cbind(mode = "known_age", thinning = 20, steps = 100000, parameter = row.names(gomp_known_age_MCMC_diag_thinSteps20), gomp_known_age_MCMC_diag_thinSteps20),
      cbind(mode = "estimation", thinning = 1, steps = 10000, parameter = row.names(gomp_anthr_MCMC_diag_thinSteps1[1:3,]), gomp_anthr_MCMC_diag_thinSteps1[1:3,]),
      cbind(mode = "estimation", thinning = 20, steps = 100000, parameter = row.names(gomp_anthr_MCMC_diag_thinSteps20[1:3,]), gomp_anthr_MCMC_diag_thinSteps20[1:3,])) -> bayes_complete
row.names(bayes_complete) = NULL
bayes_complete_table <- knitr::kable(bayes_complete, caption = paste0("Bayesian model with simulated dataset. n = ", y, 
                                              ", Gompertz \u03B2 = ", round(b_,4), ", 
                                              Gompertz \u03B1 = ", round(a_,4), ".", sep = ""))  %>% 
  kableExtra::column_spec(., 1:9, width_max = "3cm")
