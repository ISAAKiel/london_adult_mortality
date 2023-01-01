n = 500
beta = 0.05

if (runCodeNew){
  set.seed(1312)
  # modelling one population with random values
  ind_df <- lt.sampling(1, n_min = n, n_max = n, b_min = beta, b_max = beta)
  
  # Bayes models for defined settings
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
  
  # combining diagnostics into a table
  rbind(cbind(mode = "known_age", thinning = 1, steps = 10000, 
              parameter = row.names(gomp_known_age_MCMC_diag_thinSteps1), 
              gomp_known_age_MCMC_diag_thinSteps1),
        cbind(mode = "known_age", thinning = 20, steps = 100000, 
              parameter = row.names(gomp_known_age_MCMC_diag_thinSteps20), 
              gomp_known_age_MCMC_diag_thinSteps20),
        cbind(mode = "estimation", thinning = 1, steps = 10000, 
              parameter = row.names(gomp_anthr_MCMC_diag_thinSteps1[1:3,]), 
              gomp_anthr_MCMC_diag_thinSteps1[1:3,]),
        cbind(mode = "estimation", thinning = 20, steps = 100000, 
              parameter = row.names(gomp_anthr_MCMC_diag_thinSteps20[1:3,]), 
              gomp_anthr_MCMC_diag_thinSteps20[1:3,])) -> bayes_complete
  row.names(bayes_complete) <- NULL
  
  # saves results in Rda-object
  save(bayes_complete, file = file.path(".", saveFileDir, "bayes_complete.Rda") )
}
load(file.path(".", saveFileDir, "bayes_complete.Rda") )

knitr::kable(bayes_complete, 
             caption = paste0("Bayesian model with simulated dataset. n = ", 
                              n, ", Gompertz \u03B2 = ", round(beta, 4), ".", sep = ""))  %>% 
  kableExtra::column_spec(., 1:9, width_max = "3cm") -> bayes_complete_table
