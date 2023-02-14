# London data, from Landers 1997, 172 Tab. 5.5
if (runCodeNew){
  set.seed(5712)
  eng_mort <- read.table("./data/Landers_1997_London.txt", header=TRUE, sep = "\t", skip = 1)
  eng_mort <- eng_mort[-1,]
  eng_mort_melt <- reshape2::melt(eng_mort, id.vars = "Age", value.name = "qx")
  years <- unique(eng_mort_melt$variable)
  
  Landers_result <- data.frame()
  for(i in years) {
    year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
    year_data$age_mod <- year_data$Age - 15
    
    df_length <- length(year_data$age_mod)
    
    # calculation of dx
    dx <- NULL
    lx_ <- NULL
    lx <- 1
    for (k in 1: df_length ) {
      dx_1 <- year_data$qx[k] * lx
      lx <- lx - dx_1
      dx <- c(dx, dx_1)
      lx_ <- c(lx_, lx)
    }
    year_data$dx <- dx
    
    
    
    # Bayes
    year_data_uncount <- year_data %>% uncount(round(dx * 1000))
    year_data_uncount$age_end <- ifelse(year_data_uncount$Age < 60, year_data_uncount$Age + 15, year_data_uncount$Age + 40)
    
    
    gomp.anthr_age(year_data_uncount, age_beg = "Age", age_end = "age_end",
                   silent.jags = FALSE,
                   silent.runjags = FALSE,
                   thinSteps = 10,
                   numSavedSteps = 200000,
                   minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    ind_result <- cbind(year = i, parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    Landers_result <- rbind(Landers_result, ind_result )
  }
  
  # saves results in Rda-object
  save(Landers_result, file = file.path(".", saveFileDir, "Landers_result.Rda") )
}
load(file.path(".", saveFileDir, "Landers_result.Rda") )