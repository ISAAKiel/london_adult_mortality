# Data on English monks, peers and tenants from Hatcher 1986, 38 Table 5

medieval <- read.table("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/global_history/data/Hatcher_monks.txt", header=TRUE, sep = "\t")
medieval_melt <- reshape2::melt(medieval, id.vars = "Age", value.name = "qx")

group <- unique(medieval_melt$variable)

medieval_result <- data.frame()
for(i in group) {
  group_data <- medieval_melt[ which(medieval_melt$variable == i), ]
  group_data$age_mod <- group_data$Age - 20
  
  df_length <- length(group_data$age_mod)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- group_data$qx[k] * lx / 1000
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
  }
  group_data$dx <- dx
  
  group_data$death <- 1
  sample_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age_mod, death) ~ 1, 
                                          data = group_data, dist="gompertz", weights = dx)
  sample_data_lt_Gompertz_shape <- sample_data_lt$coefficients[1]
  sample_data_lt_Gompertz_rate <- exp(sample_data_lt$coefficients[2])
  
  ind_result <- cbind(group = i, beta = sample_data_lt_Gompertz_shape, alpha = sample_data_lt_Gompertz_rate)
  medieval_result <- rbind(medieval_result, ind_result )
}

rownames(medieval_result) <- NULL
cols.num <- c("beta", "alpha")
medieval_result[cols.num] <- sapply(medieval_result[cols.num],as.numeric)
year <- c(1450, 1425, 1410, 1440)
medieval_result <- data.frame(names = medieval_result$group, group = "Hatcher", year, beta = medieval_result$beta, alpha = medieval_result$alpha)
