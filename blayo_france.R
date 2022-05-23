# Blayo 1975, France
path <- ("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/global_history/data/Blayo1975.xlsx")
# read and discard incomplete columns/years
blayo_m <- readxl::read_xlsx(path = path, sheet = 3, skip = 1)[,-c(7:9)]
colnames(blayo_m) <- c("age", "1740-1749", "1750-1759", "1760-1769", "1770-1779", "1780-1789", "1820-1829")
blayo_f <- readxl::read_xlsx(path = path, sheet = 4, skip = 1)[,-c(7:9)]
colnames(blayo_f) <- c("age", "1740-1749", "1750-1759", "1760-1769", "1770-1779", "1780-1789", "1820-1829")

blayo_m_melt <- reshape2::melt(blayo_m, id.vars = c("age"), value.name = "lx")
blayo_f_melt <- reshape2::melt(blayo_f, id.vars = c("age"), value.name = "lx")
blayo <- merge(blayo_m_melt, blayo_f_melt, by= c("age", "variable"))
blayo$lx_sum <- blayo$lx.x + blayo$lx.y
blayo <- blayo[,-c(3:4)]
blayo <- blayo[order(blayo[,2], blayo[,1]) , ]

years <- unique(blayo$variable)

blayo_result <- data.frame()
for(i in years) {
  year_data <- blayo[ which(blayo$variable == i), ][-c(1:4),]
  year_data$age_mod <- year_data$age - 15
  df_length <- length(year_data$age)
  
  # calculation of dx
  dx <- NULL
  for (k in 1:(df_length - 1) ) {
    dx_1 <- year_data$lx_sum[k] - year_data$lx_sum[k+1]
    dx <- c(dx, dx_1)
  }
  year_data$dx <- c(dx, year_data$lx_sum[df_length])
  
  year_data$death <- 1
  year_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age_mod, death) ~ 1, 
                                        data = year_data, dist="gompertz", weights = dx)
  year_data_lt_Gompertz_shape <- year_data_lt$coefficients[1]
  year_data_lt_Gompertz_rate <- exp(year_data_lt$coefficients[2])
  
  year_begin <- as.numeric(substring(i, 1, 4))
  year_end <- as.numeric(substring(i, 6, 9))
  year_mid <- (year_begin + year_end) / 2
  
  ind_result <- cbind(names = i, group = "France", year = year_mid, 
                      beta = year_data_lt_Gompertz_shape, alpha = year_data_lt_Gompertz_rate)
  blayo_result <- rbind(blayo_result, ind_result )
}
rownames(blayo_result) <- NULL
cols.num <- c("year", "beta", "alpha")
blayo_result[cols.num] <- sapply(blayo_result[cols.num],as.numeric)
