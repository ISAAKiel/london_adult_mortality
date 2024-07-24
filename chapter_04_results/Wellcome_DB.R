wellcome_data <- cbind.data.frame(
  age_beg = c(NA, NA, NA, 12, 18, 26, 36, 46, 18),
  age_end = c(NA, NA, NA, 18, 26, 36, 46, 100, 100),
  bermondsey_abbey = c("Bermondsey Abbey", 1089, 1538, 1, 15, 28, 51, 37, 69),
  st_mary = c("St. Mary Graces", 1350, 1540, 24, 30, 58, 69, 40, 77),
  st_mary_spital_p14 = c("St. Mary Spital, 1120-1200", 1120, 1200, 34, 51, 104, 97, 32, 11),
  st_mary_spital_p15 = c("St. Mary Spital, 1200-1250", 1200, 1250, 60, 100, 151, 115, 33, 11),
  st_mary_spital_p16 = c("St. Mary Spital, 1250-1400", 1250, 1400, 91, 244, 414, 344, 117, 64),
  st_mary_spital_p17 = c("St. Mary Spital, 1400-1539", 1400, 1539, 36, 107, 157, 107, 48, 12),
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
    pop_rate <- psych::geometric.mean(london_sub$rate)
    
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
    
    #Bayesian model with anthropological age estimate and population growth
    gomp.anthr_age.r(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                     thinSteps = 1, minimum_age = 12,
                     maximum_age = 100,
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

reshape2::melt(wellcome_result, id.vars = c('cemetery', 'parameter')) %>%
  arrange(cemetery, parameter) -> wellcome_result_melt

unique(wellcome_result_melt$cemetery) -> well_cemetery

# start ages
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='start') %>%
  pull(value) -> well_start

# end ages
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='end') %>%
  pull(value) -> well_end

# Gompertz beta start
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='beta',variable=='HDIlow') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 4) -> well_beta_HDIlow

# Gompertz beta mode
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='beta',variable=='Mode') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 4) -> well_beta_mode

# Gompertz beta end
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='beta',variable=='HDIhigh') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 4) -> well_beta_HDIhigh

# modal age M start
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='HDIlow') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 1) -> well_M_HDIlow

# modal age M mode
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='Mode') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 1) -> well_M_mode

# modal age M end
wellcome_result_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='HDIhigh') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 1) -> well_M_HDIhigh

# expectation of life
# gomp.ex() s. helper_functions.R
wellcome_20ex <- NULL
wellcome_25ex <- NULL
for (k in well_cemetery) {
  wellcome_param <- subset(wellcome_result_melt, cemetery == k & 
                           (parameter == "alpha" | parameter == "beta") & variable == "Mode") %>%
                            pull(value) %>% as.numeric(.)
  w20ex <- gomp.ex(20, wellcome_param[1], wellcome_param[2], age_start = 12)
  w25ex <- gomp.ex(25, wellcome_param[1], wellcome_param[2], age_start = 12)
  wellcome_20ex <- c(wellcome_20ex, w20ex)
  wellcome_25ex <- c(wellcome_25ex, w25ex)
}

wellcome_overview <- data.frame(cemetery = well_cemetery,
                                beta = well_beta_mode,
                                beta_range = paste0(well_beta_HDIlow, "-", well_beta_HDIhigh), 
                                M = well_M_mode,
                                M_range = paste0(well_M_HDIlow, "-", well_M_HDIhigh), 
                                ex20 = round(wellcome_20ex, 1), 
                                ex25 = round(wellcome_25ex, 1))

wellcome_prep <- data.frame(source = "osteological",
                            data = well_cemetery,
                            M = well_M_mode,
                            year = NA,
                            start = well_start,
                            end = well_end,
                            HDIlow = well_M_HDIlow,
                            HDIhigh = well_M_HDIhigh) %>% subset(., data != "St. Bride's crypt (known age)")

############################

## the same measures for compensation of population growth
wellcome_result_r <- rbind(wellcome_result_r, stbrides_crypt_full_r)

reshape2::melt(wellcome_result_r, id.vars = c('cemetery', 'parameter')) %>%
  arrange(cemetery, parameter) -> wellcome_result_r_melt

# Gompertz beta start
wellcome_result_r_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='beta',variable=='HDIlow') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 4) -> well_beta_HDIlow_r

# Gompertz beta mode
wellcome_result_r_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='beta',variable=='Mode') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 4) -> well_beta_mode_r

# Gompertz beta end
wellcome_result_r_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='beta',variable=='HDIhigh') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 4) -> well_beta_HDIhigh_r

# modal age M start
wellcome_result_r_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='HDIlow') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 1) -> well_M_HDIlow_r

# modal age M mode
wellcome_result_r_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='Mode') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 1) -> well_M_mode_r

# modal age M end
wellcome_result_r_melt %>% 
  group_by(cemetery) %>% 
  filter(parameter =='M',variable=='HDIhigh') %>%
  pull(value) %>% as.numeric(.)  %>% round(., 1) -> well_M_HDIhigh_r

# expectation of life
# gomp.ex() s. helper_functions.R
wellcome_20ex_r <- NULL
wellcome_25ex_r <- NULL
for (k in well_cemetery) {
  wellcome_param <- subset(wellcome_result_r_melt, cemetery == k & 
                             (parameter == "alpha" | parameter == "beta") & variable == "Mode") %>%
    pull(value) %>% as.numeric(.)
  w20ex <- gomp.ex(20, wellcome_param[1], wellcome_param[2], age_start = 12)
  w25ex <- gomp.ex(25, wellcome_param[1], wellcome_param[2], age_start = 12)
  wellcome_20ex_r <- c(wellcome_20ex_r, w20ex)
  wellcome_25ex_r <- c(wellcome_25ex_r, w25ex)
}

wellcome_overview_r <- data.frame(cemetery = well_cemetery,
                                  r_beta = well_beta_mode_r,
                                  r_beta_range = paste0(well_beta_HDIlow_r, "-", well_beta_HDIhigh_r), 
                                  r_M = well_M_mode_r,
                                  r_M_range = paste0(well_M_HDIlow_r, "-", well_M_HDIhigh_r), 
                                  r_ex20 = round(wellcome_20ex_r, 1), 
                                  r_ex25 = round(wellcome_25ex_r, 1))

wellcome_prep_r <- data.frame(source = "osteological",
                              data = well_cemetery,
                              M = well_M_mode_r,
                              year = NA,
                              start = well_start,
                              end = well_end,
                              HDIlow = well_M_HDIlow_r,
                              HDIhigh = well_M_HDIhigh_r) %>% subset(., data != "St. Bride's crypt (known age)")


wellcome_overview_all <- cbind(wellcome_overview, wellcome_overview_r[,-1])