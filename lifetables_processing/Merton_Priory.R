if (runCodeNew){
  set.seed(7745)
  path <- "./data/Merton_Priory_phases.txt"
  merton <- read.table(path, header=TRUE, sep = "\t")
  age_beg <- c(13, 17, 26, 45, 17)
  age_end <- c(17, 26, 45, 100, 100)
  merton_priory_all <- c(11, 39, 293, 185, 108)
  merton_df <- data.frame(phase = merton$Phase, round(merton[,3:7] * merton$No. / 100) )
  merton_df_sum <- merton_df %>% group_by(phase) %>% 
    summarise(X13.17 = sum(X13.17), X17.26 = sum(X17.26), 
              X26.45 = sum(X26.45), X45.100 = sum(X45.100), X17.100 = sum(X17.100)) %>%
    as.data.frame(.)
  
  merton_phases_size <- merton_df %>% group_by(phase) %>% summarise(n = sum(X13.17 + X17.26+ X26.45 + X45.100 +X17.100))
  
  n <- merton_df_sum$phase
  merton_df_sum_t <- as.data.frame(t(merton_df_sum[,-1]))
  colnames(merton_df_sum_t) <- n
  rownames(merton_df_sum_t) <- NULL
  merton_priory <- data.frame(cbind(age_beg, age_end, merton_priory_all), merton_df_sum_t)
  
  molas_dating_start <- c(1117, 1117, 1222,
                          1300, 1390)
  molas_dating_end <- c(1538, 1222, 1300,
                        1390, 1538)
  
  merton_names <- colnames(merton_priory)
  
  merton_result <- data.frame()
  for (t in 1:1 ) {
    molas_ind <- data.frame(merton_priory[,1:2], dx = merton_priory[,(t + 2)])
    year_data_uncount <- molas_ind %>% uncount(dx)
    
    cem_dates <- c(molas_dating_start[t],molas_dating_end[t])
    london_sub <- subset(london_df, year >= cem_dates[1] & year < cem_dates[2])
    pop_rate <- psych::geometric.mean(london_sub$rate) - 1
    
    #Bayesian modell with anthropological age estimate
    gomp.anthr_age.r(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                   thinSteps = 1, minimum_age = 13,
                   numSavedSteps = 400000, r = pop_rate) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    ind_result <- cbind( cemetery = merton_names[t+2], start = molas_dating_start[t], end = molas_dating_end[t],
                         parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    merton_result <- rbind(merton_result, ind_result )
  }
  
  # saves results in Rda-object
  save(merton_result, file = file.path(".", saveFileDir, "merton_result.Rda") )
}
load(file.path(".", saveFileDir, "merton_result.Rda") )
