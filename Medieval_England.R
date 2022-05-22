# Data on English monks, peers and tenants from Hatcher 1986, 38 Table 5

medieval <- read.table("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/global_history/Hatcher_monks.txt", header=TRUE, sep = "\t")
medieval_melt <- reshape2::melt(medieval, id.vars = "Age", value.name = "qx")

group <- unique(medieval_melt$variable)

medieval_result <- data.frame()
for(i in group) {
  group_data <- medieval_melt[ which(medieval_melt$variable == i), ]
  group_data$age_mod <- group_data$Age - 22.5
  
  # calculation of lx
  lx_c <- NULL
  lx <- 1
  for (k in 2:length(group_data$Age)) {
    lx_1 <- lx * (1 - group_data$qx[k-1]* 0.001)
    lx_c <- c(lx_c, lx_1)
    lx <- lx_1
  }
  group_data$lx <- c(1, lx_c)
  
  # mort_fit_OLS <- lm(log(qx*0.001)  ~ age_mod, data = year_data)
  # OLS_Gompertz_shape <- mort_fit_OLS$coefficients[2]
  # OLS_Gompertz_rate <- exp(mort_fit_OLS$coefficients[1])
  
  # fit survival data with nls
  nls_estim_fit <- nls(lx ~ exp(a/b - a/b * exp(b * (Age - 20) ) ) , 
                       data = group_data, start=list(a = 0.001, b = 0.075) )
  NLS_estim_Gompertz_shape <- summary(nls_estim_fit)$coefficients[2]
  NLS_estim_Gompertz_rate <- summary(nls_estim_fit)$coefficients[1]
  
  ind_result <- cbind(group = i, beta = NLS_estim_Gompertz_shape, alpha = NLS_estim_Gompertz_rate)
  medieval_result <- rbind(medieval_result, ind_result )
}

rownames(medieval_result) <- NULL
cols.num <- c("beta", "alpha")
medieval_result[cols.num] <- sapply(medieval_result[cols.num],as.numeric)
year <- c(1450, 1425, 1410, 1440)
medieval_result <- data.frame(names = medieval_result$group, group = "Hatcher", year, beta = medieval_result$beta, alpha = medieval_result$alpha)
