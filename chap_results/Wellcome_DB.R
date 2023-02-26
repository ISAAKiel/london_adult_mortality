wellcome_data <- cbind.data.frame(
  age_beg = c(NA, NA, NA, 12, 18, 26, 36, 46, 18),
  age_end = c(NA, NA, NA, 18, 26, 36, 46, 100, 100),
  bermondsey_abbey = c("Bermondsey Abbey", 1089, 1538, 1, 15, 28, 51, 37, 69),
  st_mary = c("St. Mary Graces", 1350, 1540, 24, 30, 58, 69, 40, 77),
  st_mary_hospital_p14 = c("St. Mary Hospital, 1120-1200", 1120, 1200, 34, 51, 104, 97, 32, 11),
  st_mary_hospital_p15 = c("St. Mary Hospital, 1200-1250", 1200, 1250, 60, 100, 151, 115, 33, 11),
  st_mary_hospital_p16 = c("St. Mary Hospital, 1250-1400", 1250, 1400, 91, 244, 414, 344, 117, 64),
  st_mary_hospital_p17 = c("St. Mary Hospital, 1400-1539", 1400, 1539, 36, 107, 157, 107, 48, 12),
  new_churchyard = c("New Churchyard", 1569, 1739, 53, 126, 153, 132, 66, 26),
  st_benet = c("St. Benet Sherehog", 1670, 1740, 12, 9, 33, 50, 32, 43),
  chelsea_old_church = c("Chelsea Old church", 1712, 1842, 3, 14, 17, 46, 72, 16),
  st_marylebone = c("St. Marylebone", 1742, 1817, 3, 20, 42, 69, 52, 40),
  st_marylebone_north = c("St. Marylebone Paddington Street north", 1772, 1853, 2, 9, 33, 86, 50, 52),
  st_brides_lower = c("St. Bride's lower churchyard", 1770, 1849, 10, 10, 44, 88, 162, 65),
  sheen_burial_ground = c("Sheen's burial ground", 1763, 1854, 6, 13, 26, 32, 41, 54),
  bow_baptist = c("Bow Baptist Church", 1816, 1854, 17, 25, 51, 64, 50, 24),
  st_mary_and_michael = c("St. Mary and St. Michael", 1843, 1853, 16, 33, 81, 92, 41, 21)
)

if (runCodeNew){
  set.seed(982)
  wellcome_result_r <- data.frame()
  wellcome_result <- data.frame()
  for (t in 3:length(wellcome_data) ) {
    molas_ind <- data.frame(wellcome_data[-c(1:3),1:2], dx = wellcome_data[-c(1:3),t])
    year_data_uncount <- molas_ind %>% uncount(as.numeric(dx))
    
    cem_dates <- c(wellcome_data[2,t], wellcome_data[3, t])
    london_sub <- subset(london_df, year >= cem_dates[1] & year < cem_dates[2])
    pop_rate <- psych::geometric.mean(london_sub$rate) - 1
    
    #Bayesian modell with anthropological age estimate
    gomp.anthr_age(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                     thinSteps = 1, minimum_age = 12,
                     numSavedSteps = 400000) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind( cemetery = wellcome_data[1, t], 
                           start = wellcome_data[2, t], 
                           end = wellcome_data[3, t], 
                           parameter = c("alpha", "beta", "M"), 
                           gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    wellcome_result <- rbind(wellcome_result, ind_result )
    
    #Bayesian modell with anthropological age estimate and population growth
    gomp.anthr_age.r(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                     thinSteps = 1, minimum_age = 12,
                     numSavedSteps = 400000, r = pop_rate) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag_r
    
    ind_result_r <- cbind( cemetery = wellcome_data[1, t], 
                         start = wellcome_data[2, t], 
                         end = wellcome_data[3, t], 
                         parameter = c("alpha", "beta", "M", "rate"), 
                         gomp_anthr_MCMC_diag_r[c(1, 2, 3, 5),])
    rownames(ind_result_r) <- NULL
    wellcome_result_r <- rbind(wellcome_result_r, ind_result_r )
    
  }
  
  # saves results in Rda-object
  save(wellcome_result, file = file.path(".", saveFileDir, "Wellcome_result.Rda") )
  save(wellcome_result_r, file = file.path(".", saveFileDir, "Wellcome_result_r.Rda") )
}
load(file.path(".", saveFileDir, "Wellcome_result.Rda") )
load(file.path(".", saveFileDir, "Wellcome_result_r.Rda") )

wellcome_result <- rbind(wellcome_result, stbrides_crypt_full)

# trying alternative way for all the subsequent steps

reshape2::melt(wellcome_result, id.vars = c('cemetery', 'parameter')) %>%
  arrange(cemetery, parameter) -> wellcome_result_melt

unique(wellcome_result_melt$cemetery) -> well_cemeter
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='start') %>%
  pull(value) -> well_start

# range of Gompertz beta values
beta_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIlow, 4), "-",
                     round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIhigh, 4) )
# range of age modes M
M_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIlow, 1), "-",
                  round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIhigh, 1) )

# expectation of life
wellcome_names <- unique(wellcome_result$cemetery)
wellcome_20ex <- NULL
wellcome_25ex <- NULL
for (k in wellcome_names) {
  wellcome_sub <- subset(wellcome_result, cemetery == k)
  w20ex <- gomp.ex(20, wellcome_sub[1,9], wellcome_sub[2,9], age_start = 12)
  w25ex <- gomp.ex(25, wellcome_sub[1,9], wellcome_sub[2,9])
  wellcome_20ex <- c(wellcome_20ex, w20ex)
  wellcome_25ex <- c(wellcome_25ex, w25ex)
}

wellcome_overview <- data.frame(cemetery = c(as.character(wellcome_data[1, -c(1:2)]), "St. Bride's crypt (known age)", "St. Bride's crypt (osteological estimates)"),
                                beta = round(wellcome_result[which(wellcome_result$parameter == "beta"),]$Mode, 4),
                                beta_range, 
                                M = round(wellcome_result[which(wellcome_result$parameter == "M"),]$Mode, 1), M_range, 
                                ex20 = round(wellcome_20ex, 1), ex25 = round(wellcome_25ex, 1))
wellcome_subset <- data.frame(cemetery = c(as.character(wellcome_data[1, -c(1:2)]), "St. Bride's crypt (estimates)"),
                              wellcome_result[which(wellcome_result$parameter == "M" & wellcome_result$cemetery != "St. Bride's crypt (known age)"),])
wellcome_prep <- data.frame(source = "osteological", wellcome_subset[,c(1, 10)], year = NA,
                            wellcome_subset[,c(3, 4, 14, 15)])
colnames(wellcome_prep) <- c("source", "data", "M", "year", "start", "end", "HDIlow", "HDIhigh")

wellcome_subset_beta <- data.frame(cemetery = c(wellcome_data[1, -c(1:2)], "St. Bride's crypt (estimates)"),
                                   wellcome_result[which(wellcome_result$parameter == "beta" & wellcome_result$cemetery != "St. Bride's crypt (known age)"),])
wellcome_prep_beta <- data.frame(source = "osteological", wellcome_subset_beta[,c(1, 10)], year = NA,
                                 wellcome_subset_beta[,c(3, 4, 14, 15)])
colnames(wellcome_prep_beta) <- c("source", "data", "beta", "year", "start", "end", "HDIlow", "HDIhigh")


## the same measures for compensation of population growth
wellcome_result_r <- rbind(wellcome_result_r, stbrides_crypt_full_r)

# range of Gompertz beta values
beta_range_r <- paste0(round(wellcome_result_r[which(wellcome_result_r$parameter == "beta"),]$HDIlow, 4), "-",
                     round(wellcome_result_r[which(wellcome_result_r$parameter == "beta"),]$HDIhigh, 4) )
# range of age modes M
M_range_r <- paste0(round(wellcome_result_r[which(wellcome_result_r$parameter == "M"),]$HDIlow, 1), "-",
                  round(wellcome_result_r[which(wellcome_result_r$parameter == "M"),]$HDIhigh, 1) )
# range of rate
rate_range <- paste0(round(wellcome_result_r[which(wellcome_result_r$parameter == "rate"),]$HDIlow, 3), "-",
                     round(wellcome_result_r[which(wellcome_result_r$parameter == "rate"),]$HDIhigh, 3) )

# expectation of life
wellcome_names_r <- unique(wellcome_result_r$cemetery)
wellcome_20ex_r <- NULL
wellcome_25ex_r <- NULL
for (k in wellcome_names_r) {
  wellcome_sub <- subset(wellcome_result_r, cemetery == k)
  w20ex <- gomp.ex(20, wellcome_sub[1,9], wellcome_sub[2,9], age_start = 12)
  w25ex <- gomp.ex(25, wellcome_sub[1,9], wellcome_sub[2,9])
  wellcome_20ex_r <- c(wellcome_20ex_r, w20ex)
  wellcome_25ex_r <- c(wellcome_25ex_r, w25ex)
}

wellcome_overview_r <- data.frame(cemetery = c(as.character(wellcome_data[1, -c(1:2)]), "St. Bride's crypt (known age)", "St. Bride's crypt (osteological estimates)"),
                                beta_r = round(wellcome_result_r[which(wellcome_result_r$parameter == "beta"),]$Mode, 4),
                                beta_range_r, 
                                M_r = round(wellcome_result_r[which(wellcome_result_r$parameter == "M"),]$Mode, 1), M_range_r, 
                                rate = round(wellcome_result_r[which(wellcome_result_r$parameter == "rate"),]$Mode, 3), rate_range,
                                ex20_r = round(wellcome_20ex_r, 1), ex25_r = round(wellcome_25ex_r, 1))
wellcome_subset_r <- data.frame(cemetery = c(as.character(wellcome_data[1, -c(1:2)]), "St. Bride's crypt (estimates)"),
                              wellcome_result_r[which(wellcome_result_r$parameter == "M" & wellcome_result_r$cemetery != "St. Bride's crypt (known age)"),])
wellcome_prep_r <- data.frame(source = "osteological", wellcome_subset_r[,c(1, 10)], year = NA,
                            wellcome_subset_r[,c(3, 4, 14, 15)])
colnames(wellcome_prep_r) <- c("source", "data", "M", "year", "start", "end", "HDIlow", "HDIhigh")

wellcome_overview_all <- cbind(wellcome_overview, wellcome_overview_r[,-1])
