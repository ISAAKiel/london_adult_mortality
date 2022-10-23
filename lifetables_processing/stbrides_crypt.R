# St. Bride's crypt
  molas_data <- c("../Wellcome_Database/ST\ BRIDES\ CRYPT_SB79_DENTAL\ DATA_NILS\ MUELLER-SCHEESSEL_SEPT\ 2021.xlsx")
  my_data3 <- readxl::read_excel(molas_data, sheet = 3) [, 1:8]
  colnames(my_data3) <- c("site", "ind", "birth", "death", "known_sex", "known_age", "sex", "age")
  stbrides <- as.data.frame(my_data3)
  stbrides$known_age <- as.integer(stbrides$known_age)
  stbrides$birth <- as.integer(stbrides$birth)
  stbrides$death <- as.integer(stbrides$death)
  stbrides <- na.omit(stbrides)
  stbrides <- subset(stbrides, known_age >= 12 & ind != 105)
  
  length_stbrides <- nrow(stbrides)
  for (i in 1:length_stbrides) {
    if(stbrides$age[i] == 6) {
      stbrides$age_beg[i] <-  12
      stbrides$age_end[i] <-  18
    } else if(stbrides$age[i] == 7) {
      stbrides$age_beg[i] <-  18
      stbrides$age_end[i] <-  26
    } else if(stbrides$age[i] == 8) {
      stbrides$age_beg[i] <-  26
      stbrides$age_end[i] <-  36
    } else if(stbrides$age[i] == 9) {
      stbrides$age_beg[i] <-  36
      stbrides$age_end[i] <-  46
    } else if(stbrides$age[i] == 10) {
      stbrides$age_beg[i] <-  46
      stbrides$age_end[i] <-  100
    } else if(stbrides$age[i] == 11) {
      stbrides$age_beg[i] <-  18
      stbrides$age_end[i] <-  100
    } 
  }
  
  if (runCodeNew){
    set.seed(92311)
    stbrides_crypt_result <- NA
  #Bayesian modell with anthropological age estimate
    cem_dates <- c(1740, 1853)
    london_sub <- subset(london_df, year >= cem_dates[1] & year < cem_dates[2])
    pop_rate <- psych::geometric.mean(london_sub$rate) - 1
  
  gomp.anthr_age.r(stbrides, age_beg = "age_beg", age_end = "age_end",
                 thinSteps = 1, minimum_age = 12,
                 numSavedSteps = 300000, r = pop_rate) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  
  gomp.known_age.r(stbrides, known_age = "known_age",
                 thinSteps = 1,
                 numSavedSteps = 200000, minimum_age = 12, r = pop_rate) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_known_age_MCMC_diag
  
  stbrides_crypt_full <- rbind(cbind(cemetery = "St. Bride's crypt (known age)", 
                                     start = 1740, end = 1853,
                                     parameter = c("alpha", "beta", "M"), gomp_known_age_MCMC_diag[1:3,]), 
                               cbind(cemetery = "St. Bride's crypt (estimates)",
                                     start = 1740, end = 1853,
                                     parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,]))
  rownames(stbrides_crypt_full) <- NULL
  
  # saves results in Rda-object
  save(stbrides_crypt_full, file = file.path(".", saveFileDir, "stbrides_crypt_full.Rda") )
}
load(file.path(".", saveFileDir, "stbrides_crypt_full.Rda") )

stbrides_crypt_plot <- ggplot() + xlim(12, 100) + geom_density(data = stbrides, aes(x=known_age), bw=5)+
  geom_function(fun = function(x) flexsurv::dgompertz(x - 12, stbrides_crypt_full[5,9], stbrides_crypt_full[4,9]), colour = "red") +
  geom_function(fun = function(x) flexsurv::dgompertz(x - 12, stbrides_crypt_full[2,9],  stbrides_crypt_full[1,9]), colour= "blue") +
  xlab("age") + ylab("density")

# ggplot(stbrides) + geom_linerange(aes(y = known_age, xmin = age_beg, xmax = age_end), alpha=0.4)
# ggplot(stbrides) + geom_density(aes(x=birth)) + geom_density(aes(x=death))