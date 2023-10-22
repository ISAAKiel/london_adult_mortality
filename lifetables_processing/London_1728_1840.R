# mortality bills 1728-1840, from Roberts/Cox 2003, 304 Table 6.5; > 100 years and < 1 collapsed

if (runCodeNew){
  set.seed(512)
  eng_mort <- read.table("./data/Mortality_bills_1728_1840.txt", header=TRUE, sep = "\t", skip = 1)
  eng_mort_melt <- reshape2::melt(eng_mort[-c(1, 2, 3, 4, 5),], id.vars = "Decade", value.name = "Dx")
  years <- unique(eng_mort_melt$variable)
  
  london_1728_1840_result <- data.frame()
  london_1728_1840_result_r <- data.frame()
  for(i in years) {
    year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
    year_data_uncount <- year_data %>% uncount(round(Dx * 10))
    year_data_uncount$age_beg <- as.numeric(substr(year_data_uncount$Decade, 1, 2)) - 1
    year_data_uncount$age_end <- year_data_uncount$age_beg + 10
    if(i == "X1728") {
      cem_dates <- c(1728, 1730)
    } else {
    cem_dates <- c(as.numeric(substr(i,2,5)), as.numeric(substr(i,2,5)) + 10)
    }
    london_sub <- subset(london_df, year >= cem_dates[1] & year < cem_dates[2])
    pop_rate <- psych::geometric.mean(london_sub$rate)
    
    # including population growth
    gomp.anthr_age.r(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                     thinSteps = 1,
                     numSavedSteps = 200000, 
                     minimum_age = 20, 
                     maximum_age = 100,
                     r = pop_rate) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag_r

    # without population growth
      gomp.anthr_age(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                     silent.jags = FALSE,
                     silent.runjags = FALSE,
                     thinSteps = 1,
                     numSavedSteps = 200000,
                     minimum_age = 20) %>%
        diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind(year = i, parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    london_1728_1840_result <- rbind(london_1728_1840_result, ind_result )
    
    ind_result_r <- cbind(year = i, 
    parameter = c("alpha", "beta", "M", "rate"), 
    gomp_anthr_MCMC_diag_r[c(1, 2, 3, 5),])
    rownames(ind_result_r) <- NULL
    london_1728_1840_result_r <- rbind(london_1728_1840_result_r, ind_result_r )
  }
  # saves results in Rda-object
  save(london_1728_1840_result, file = file.path(".", saveFileDir, "london_1728_1840_result.Rda") )
  save(london_1728_1840_result_r, file = file.path(".", saveFileDir, "london_1728_1840_result_r.Rda") )
}
load(file.path(".", saveFileDir, "london_1728_1840_result.Rda") )
load(file.path(".", saveFileDir, "london_1728_1840_result_r.Rda") )

# range of Gompertz beta values
beta_range <- paste0(round(min(london_1728_1840_result[which(london_1728_1840_result$parameter == "beta"),]$Mode), 4), "-",
                     round(max(london_1728_1840_result[which(london_1728_1840_result$parameter == "beta"),]$Mode) , 4) )
# range of age modes M
M_range <- paste0(round(min(london_1728_1840_result[which(london_1728_1840_result$parameter == "M"),]$Mode), 1), "-",
                  round(max(london_1728_1840_result[which(london_1728_1840_result$parameter == "M"),]$Mode), 1) )

london_1728_1840_ranges <- data.frame(parameter = c("beta", "M"), ranges = c(beta_range, M_range))

## parameters with population growth
# range of Gompertz beta values
beta_range_r <- paste0(round(min(london_1728_1840_result_r[which(london_1728_1840_result_r$parameter == "beta"),]$Mode), 4), "-",
                     round(max(london_1728_1840_result_r[which(london_1728_1840_result_r$parameter == "beta"),]$Mode) , 4) )
# range of age modes M
M_range_r <- paste0(round(min(london_1728_1840_result_r[which(london_1728_1840_result_r$parameter == "M"),]$Mode), 1), "-",
                  round(max(london_1728_1840_result_r[which(london_1728_1840_result_r$parameter == "M"),]$Mode), 1) )
# range of rate
rate_range <- paste0(round(min(london_1728_1840_result_r[which(london_1728_1840_result_r$parameter == "rate"),]$Mode), 3), "-",
                     round(max(london_1728_1840_result_r[which(london_1728_1840_result_r$parameter == "rate"),]$Mode), 3) )

london_1728_1840_ranges_r <- data.frame(parameter = c("beta_r", "M_r", "r"), ranges = c(beta_range_r, M_range_r, rate_range))
