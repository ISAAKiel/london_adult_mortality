# London data, from Landers 1997, 180 Tab. 5.8
if (runCodeNew){
  set.seed(98)
  eng_mort <- read.table("./data/Landers_1997_London_series.txt", header=TRUE, sep = "\t")
  eng_mort <- eng_mort[-c(1:4),]
  eng_mort_melt <- reshape2::melt(eng_mort, id.vars = "age", value.name = "dx")
  
  years <- unique(eng_mort_melt$variable)
  
  London_Landers_result <- data.frame()
  for(i in years) {
    year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
    year_data$age_mod <- year_data$age - 20
    
    # Bayes
    year_data_uncount <- year_data %>% uncount(dx)
    year_data_uncount$age_end <- ifelse(year_data_uncount$age < 70, year_data_uncount$age + 10, year_data_uncount$age + 30)
    
    gomp.anthr_age(year_data_uncount, age_beg = "age", age_end = "age_end",
                   silent.jags = TRUE,
                   silent.runjags = TRUE,
                   thinSteps = 1,
                   numSavedSteps = 400000,
                   minimum_age = 20) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind(year = i, parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    London_Landers_result <- rbind(London_Landers_result, ind_result )
  }
  # saves results in Rda-object
  save(London_Landers_result, file = file.path(".", saveFileDir, "London_Landers_result.Rda") )
}
load(file.path(".", saveFileDir, "London_Landers_result.Rda") )

# range of Gompertz beta values
beta_range <- paste0(round(min(London_Landers_result[which(London_Landers_result$parameter == "beta"),]$Mode), 4), "-",
       round(max(London_Landers_result[which(London_Landers_result$parameter == "beta"),]$Mode) , 4) )
# range of age modes M
M_range <- paste0(round(min(London_Landers_result[which(London_Landers_result$parameter == "M"),]$Mode), 1), "-",
       round(max(London_Landers_result[which(London_Landers_result$parameter == "M"),]$Mode), 1) )

London_series_ranges <- data.frame(parameter = c("beta", "M"), ranges = c(beta_range, M_range))
