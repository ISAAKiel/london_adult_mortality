# data from Zhao 1997
china_wang <- data.frame(Age = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95), 
                         Dx = c(69, 95, 127, 151, 180, 239, 329, 374, 438, 467, 502, 443, 362, 227, 136, 53, 13) )
china_wang$death <- 1
china_wang_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 15, death) ~ 1, 
                                        data = china_wang, dist="gompertz", weights = Dx)
china_wang_lt_Gompertz_shape <- china_wang_lt$coefficients[1]
china_wang_lt_Gompertz_rate <- exp(china_wang_lt$coefficients[2])
