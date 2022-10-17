HMD_username <- readline(prompt = "Enter username: ")
HMD_password <- readline(prompt="Enter password: ")
credentials <- c(HMD_username, HMD_password)

HMDHFDplus::getHMDitemavail("SWE", credentials[1], credentials[2])

# comparing Gompertz beta values for Dx and dx
swe_dx <- HMDHFDplus::readHMDweb("SWE", "bltper_5x10", credentials[1], credentials[2])
swe_Dx <- HMDHFDplus::readHMDweb("SWE", "Deaths_5x10", credentials[1], credentials[2])
swe_pop <- HMDHFDplus::readHMDweb("SWE", "Population5", credentials[1], credentials[2])

lt_result_dx <- data.frame()
years <- unique(swe_dx[which(swe_dx$Year > 1799 & swe_dx$Year < 1850),]$Year)
for(i in years) {
  year_data <- swe_dx[ which(swe_dx$Year == i), ]
  year_data <- subset(year_data, Age >= 15)
  year_data_uncount <- year_data %>% uncount(round(dx / 100))
  year_data_uncount$age_end <- year_data_uncount$Age + 5
  bayes_anthr_gomp_b <- NA
  bayes_anthr_gomp_a <- NA
    gomp.anthr_age(year_data_uncount, age_beg = "Age", age_end = "age_end",
                   silent.jags = FALSE,
                   silent.runjags = FALSE,
                   thinSteps = 1,
                   numSavedSteps = 10000,
                   minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    ind_result <- cbind(year = i, parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    lt_result_dx <- rbind(lt_result_dx, ind_result )
}

lt_result_Dx <- data.frame()
years <- unique(swe_Dx[which(swe_Dx$Year > 1799 & swe_Dx$Year < 1850),]$Year)
counter <- 0
for(i in years) {
  counter <- counter + 1
  #r <- lt_result_pop$rate[counter]
  year_data <- swe_Dx[ which(swe_Dx$Year == i), ]
  year_data <- subset(year_data, Age >= 15)
  year_data_uncount <- year_data %>% uncount(round(Total / 1000))
  year_data_uncount$age_end <- year_data_uncount$Age + 5
  bayes_anthr_gomp_b <- NA
  bayes_anthr_gomp_a <- NA
  gomp.anthr_age.r(year_data_uncount, age_beg = "Age", age_end = "age_end",
                 silent.jags = TRUE,
                 silent.runjags = TRUE,
                 thinSteps = 1,
                 numSavedSteps = 10000,
                 minimum_age = 15, 
                 r = -0.02) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  ind_result <- cbind(year = i, parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
  rownames(ind_result) <- NULL
  lt_result_Dx <- rbind(lt_result_Dx, ind_result )
}

## poisson
lt_result_Dx <- data.frame()
years <- unique(swe_Dx[which(swe_Dx$Year > 1799 & swe_Dx$Year < 1850),]$Year)
#years <- unique(swe_Dx[which(swe_Dx$Year == 1850),]$Year)
counter <- 0
for(i in years) {
  counter <- counter + 1
  #r <- lt_result_pop$rate[counter]
  year_data <- swe_Dx[ which(swe_Dx$Year == i), ]
  year_data <- subset(year_data, Age >= 15)
  year_data$age_end <- year_data$Age + 5
  bayes_anthr_gomp_b <- NA
  bayes_anthr_gomp_a <- NA
  poisson.interval.r(year_data, age_beg = "Age", age_end = "age_end", Dx = "Total",
                   silent.jags = TRUE,
                   silent.runjags = TRUE,
                   thinSteps = 1,
                   numSavedSteps = 10000,
                   minimum_age = 15,
                   r = 0.0) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  ind_result <- cbind(year = i, parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
  rownames(ind_result) <- NULL
  lt_result_Dx <- rbind(lt_result_Dx, ind_result )
}

lt_result_pop <- data.frame()
#years <- unique(swe_Dx$Year)
years <- unique(swe_Dx[which(swe_Dx$Year > 1799 & swe_Dx$Year < 1860),]$Year)
for(i in years) {
  year_data <- swe_pop[ which(swe_pop$Year == i), ]
  pop_sum <- sum(year_data$Total2)
  pop_result <- c(year = i, pop = pop_sum)
  lt_result_pop <- rbind(lt_result_pop, pop_result )
}
colnames(lt_result_pop) <- c("year", "pop")
lt_result_pop$rate <- c(log(lt_result_pop$pop[-1] / lt_result_pop$pop[-length(years)]) / 10, NA )
lt_result_pop <- lt_result_pop[-length(years),]

swe_e15 <- swe_dx[which(swe_dx$Age == 15),]
colnames(swe_e15) <- c("year", "age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex","OpenInterval")

merge_1 <- merge(lt_result_Dx, lt_result_dx, by="year")
merge_2 <- merge(merge_1, lt_result_pop, by="year"
merge_swe <- merge(merge_2, swe_e15, by="year")