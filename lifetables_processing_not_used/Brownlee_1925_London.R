eng_mort <- read.table("./data/Brownlee_1925_London.txt", header=TRUE, sep = "\t")
eng_mort <- eng_mort[-1,1:8]
eng_mort_melt <- reshape2::melt(eng_mort, id.vars = "age", value.name = "mx")

years <- unique(eng_mort_melt$variable)

eng_mort_result <- data.frame()
for(i in years) {
  year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
  year_data$age_mod <- year_data$age - 20
  
  df_length <- length(year_data$age_mod)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  qx <- NULL
  for (k in 1: df_length ) {
    mx <- year_data$mx[k] / 1000
    qx_1 <- 10 * mx / (1 + 5 * mx)
    dx_1 <- qx_1 * lx
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
    qx <- c(qx, qx_1)
  }
  year_data$dx <- dx
  year_data$qx <- qx
  
  year_data$death <- 1
  sample_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age_mod, death) ~ 1, 
                                          data = year_data, dist="gompertz", weights = dx)
  sample_data_lt_Gompertz_shape <- sample_data_lt$coefficients[1]
  sample_data_lt_Gompertz_rate <- exp(sample_data_lt$coefficients[2])
  
  ind_result <- cbind(year = substring(i, 2, 5), beta = sample_data_lt_Gompertz_shape, alpha = sample_data_lt_Gompertz_rate)
  eng_mort_result <- rbind(eng_mort_result, ind_result )
}

rownames(eng_mort_result) <- NULL
cols.num <- c("year", "beta", "alpha")
eng_mort_result[cols.num] <- sapply(eng_mort_result[cols.num],as.numeric)
