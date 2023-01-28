# # Graunt lifetable from Wachter 2014, 163 Table 7.2, but not useable
age <- c(16, 26, 36, 46, 56, 66, 76)
lx <- c(40, 25, 16, 10, 6, 3, 1)
qx <- c(0.375, 0.36, 0.375, 0.4, 0.5, 0.667, 1)
lx <- lx/40
dx <- lx - c(lx[-1], 0)
graunt_df <- data.frame(age, lx, qx, dx, death = 1)

graunt_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age - 16, death) ~ 1,
                                   data = graunt_df, dist="gompertz", weights = dx)
graunt_lt_Gompertz_shape <- graunt_lt$coefficients[1]
graunt_lt_Gompertz_rate <- exp(graunt_lt$coefficients[2])

mort_fit_OLS <- lm(log(qx/10)  ~ age, data = graunt_df)
OLS_Gompertz_shape <- mort_fit_OLS$coefficients[2]
OLS_Gompertz_rate <- exp(mort_fit_OLS$coefficients[1])

# 
# # fit survival data with nls
# NLS_estim_Gompertz_shape <- NA
# NLS_estim_Gompertz_rate <- NA
# tryCatch({
#   nls_estim_fit <- nls(lx ~ exp(a/b - a/b * exp(b * (age - 16) ) ) , 
#                        data = graunt_df, start=list(a = 0.001, b = 0.075), weights = dx)#, method="Nelder-Mead")
#   NLS_estim_Gompertz_shape <- summary(nls_estim_fit)$coefficients[2]
#   NLS_estim_Gompertz_rate <- summary(nls_estim_fit)$coefficients[1]
# }, error=function(e){})

# Halley lifetable
halley_age <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84)
halley_lx <- c(1000, 855, 798, 760, 732, 710, 692, 680, 670, 661, 653, 646, 640, 634, 628, 622, 616, 610, 604, 598, 592, 585, 579, 573, 567, 560, 553, 546, 539, 531, 523, 515, 507, 499, 490, 481, 472, 463, 454, 445, 436, 427, 419, 409, 397, 387, 377, 367, 357, 346, 335, 324, 313, 302, 292, 282, 272, 262, 252, 242, 232, 222, 212, 202, 192, 182, 172, 162, 152, 142, 131, 120, 109, 98, 88, 78, 68, 58, 49, 41, 34, 28, 23, 20)

halley_df <- data.frame(halley_age, halley_lx, dx)[-c(1:14),]
halley_df$halley_lx_ <- halley_df$halley_lx/628
halley_df$dx <- halley_df$halley_lx - c(halley_df$halley_lx[-1], 19)
halley_df$death <-  1

nls_estim_fit <- nls(halley_lx_ ~ exp(a/b - a/b * exp(b * (halley_age - 15) ) ) , 
                     data = halley_df, start=list(a = 0.001, b = 0.075))#, method="Nelder-Mead")
NLS_estim_Gompertz_shape <- summary(nls_estim_fit)$coefficients[2]
NLS_estim_Gompertz_rate <- summary(nls_estim_fit)$coefficients[1]
halley_result <- data.frame(names = "Breslau", group = "Halley", year = 1691, beta = NLS_estim_Gompertz_shape, alpha = NLS_estim_Gompertz_rate)

halley_lt <- flexsurv::flexsurvreg(formula = survival::Surv(halley_age - 15, death) ~ 1,
                                   data = halley_df, dist="gompertz", weights = dx)
halley_lt_Gompertz_shape <- halley_lt$coefficients[1]
halley_lt_Gompertz_rate <- exp(halley_lt$coefficients[2])
