age_beg <- c(12, 18, 26, 36, 46, 18)
age_end <- c(18, 26, 36, 46, 120, 120)
st_marylebone <- c(3, 20, 42, 69, 52, 40)
st_marylebone_north <-  c(2, 9, 33, 86, 50, 52)
marylebone_data <- data.frame(age_beg, age_end,  st_marylebone, st_marylebone_north)

if (runCodeNew){
  set.seed(662)
  marylebone_result <- data.frame()
  for (t in 3:length(marylebone_data) ) {
    molas_ind <- data.frame(marylebone_data[,1:2], dx = marylebone_data[,t])
    year_data_uncount <- molas_ind %>% uncount(as.numeric(dx))
    
    gomp.anthr_age.r(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                     thinSteps = 1, minimum_age = 12,
                     numSavedSteps = 200000, r = 0.0275) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind( cemetery = marylebone_data[1, t], 
                         parameter = c("alpha", "beta", "M", "unity", "rate"), 
                         gomp_anthr_MCMC_diag[1:5,])
    rownames(ind_result) <- NULL
    marylebone_result <- rbind(marylebone_result, ind_result )
    
  }
  
  # saves results in Rda-object
  save(marylebone_result, file = file.path(".", saveFileDir, "Marylebone_result.Rda") )
}
load(file.path(".", saveFileDir, "Marylebone_result.Rda") )

# mode values
modes <- marylebone_result[c(2,3,7,8),]$Mode

# range of Gompertz beta values
beta_range1 <- paste0(format(round(marylebone_result[2,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                     format(round(marylebone_result[2,]$HDIhigh, digits = 4), nsmall = 4  ) )
beta_range2 <- paste0(format(round(marylebone_result[7,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                      format(round(marylebone_result[7,]$HDIhigh, digits = 4), nsmall = 4  ) )
# range of age modes M
M_range1 <- paste0(format(round(marylebone_result[3,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                  format(round(marylebone_result[3,]$HDIhigh, digits = 1), nsmall = 1 ) )
M_range2 <- paste0(format(round(marylebone_result[8,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                   format(round(marylebone_result[8,]$HDIhigh, digits = 1), nsmall = 1 ) )

Marylebone_ranges <- data.frame(parameter = c("Marylebone beta", "Marylebone M", "Marylebone north beta", "Marylebone north M"), 
                                 modes = format(round(c(modes), 4), nsmall = 4), 
                                 HDI.ranges = c(beta_range1, M_range1, beta_range2, M_range2))
