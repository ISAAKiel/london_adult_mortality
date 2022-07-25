#English data, from Wrigley et al. 1997, 290 Tab. 6.19 (Adult mortality, sexes combined (1000qx)), 1640-1809, for age groups 25-84 years
#eng_mort <- read.table(file.choose(), header=TRUE, sep = "\t")
eng_mort <- read.table("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/global_history/data/wrigley_et_al_1997_england_1640-1809.txt", header=TRUE, sep = "\t")

eng_mort_melt <- reshape2::melt(eng_mort, id.vars = "Age", value.name = "qx")

years <- unique(eng_mort_melt$variable)

eng_mort_result <- data.frame()
for(i in years) {
  year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
  year_data$age_mod <- year_data$Age - 25
  
  df_length <- length(year_data$age_mod)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- year_data$qx[k] * lx / 1000
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
  }
  year_data$dx <- dx

  year_data$death <- 1
  sample_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age_mod, death) ~ 1, 
                                          data = year_data, dist="gompertz", weights = dx)
  sample_data_lt_Gompertz_shape <- sample_data_lt$coefficients[1]
  sample_data_lt_Gompertz_rate <- exp(sample_data_lt$coefficients[2])
  
  ind_result <- cbind(year = substring(i, 2), beta = sample_data_lt_Gompertz_shape, alpha = sample_data_lt_Gompertz_rate)
  eng_mort_result <- rbind(eng_mort_result, ind_result )
}

rownames(eng_mort_result) <- NULL
cols.num <- c("year", "beta", "alpha")
eng_mort_result[cols.num] <- sapply(eng_mort_result[cols.num],as.numeric)
