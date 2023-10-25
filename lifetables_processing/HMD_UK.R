if (runCodeNew){
  set.seed(689)
  if (length(credentials) > 0) {
  
  # get dx
  uk_dx <- HMDHFDplus::readHMDweb("GBRTENW", "bltper_5x5", credentials[1], credentials[2])
  
  HMD_UK_result <- data.frame()
  years <- unique(uk_dx$Year)
  years <- years[years <= 1900]
  for(i in years) {
    year_data <- uk_dx[ which(uk_dx$Year == i), ]
    year_data <- subset(year_data, Age >= 15)
    
    mort_df_x_uncount <- year_data %>% uncount(round(dx / 100))
    mort_df_x_uncount$Age_end <-  mort_df_x_uncount$Age + 5
    gomp.anthr_age(mort_df_x_uncount, age_beg = "Age", age_end = "Age_end",
                   silent.jags = FALSE,
                   silent.runjags = FALSE,
                   thinSteps = 1,
                   numSavedSteps = 300000,
                   minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind(year =  paste0("X", i), parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    
    HMD_UK_result <- rbind(HMD_UK_result, ind_result )
  }
  # saves results in Rda-object
  save(HMD_UK_result, file = file.path(".", saveFileDir, "HMD_UK_result.Rda") )
  }
} else {
  infotext <- paste ("Please enter valid credentials",
                     "for the Human Mortality Database.",
                     sep=" ")
  warning(infotext)
}
load(file.path(".", saveFileDir, "HMD_UK_result.Rda") )

# range of Gompertz beta values
beta_range <- paste0(round(min(HMD_UK_result[which(HMD_UK_result$parameter == "beta"),]$Mode), 4), "-",
                     round(max(HMD_UK_result[which(HMD_UK_result$parameter == "beta"),]$Mode) , 4) )
# range of age modes M
M_range <- paste0(round(min(HMD_UK_result[which(HMD_UK_result$parameter == "M"),]$Mode), 1), "-",
                  round(max(HMD_UK_result[which(HMD_UK_result$parameter == "M"),]$Mode), 1) )

HMD_UK_ranges <- data.frame(parameter = c("beta", "M"), ranges = c(beta_range, M_range))
