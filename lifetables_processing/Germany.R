# # data from Kessler 1992, 67 Tab. 7a (sexes pooled togehter), 1764--1799, city of Radolfzell
# radolfzell <- data.frame(Age = c(21, 31, 41, 51, 61, 71), Dx = c(43, 52, 66, 103, 138, 181) )
# radolfzell$death <- 1
# radolfzell_surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 21, death) ~ 1, 
#                                             data = radolfzell, dist="gompertz", weights = Dx)
# radolfzell_surv_lt_Gompertz_shape <- radolfzell_surv_lt$coefficients[1]
# radolfzell_surv_lt_Gompertz_rate <- exp(radolfzell_surv_lt$coefficients[2])
# 
# # compute life table
# radolfzell$age_1 <- radolfzell$Age + 1
# mort_prep_x <- mortAAR::prep.life.table(radolfzell, dec = "Dx", agebeg = "Age", ageend = "age_1", agerange = "exclude",
#                                         method = c(5,5,5,6,10,10,10,10,10,10))
# mort_life_x <- mortAAR::life.table(mort_prep_x)
# x_length <- length(mort_life_x$x)
# x_a <- cumsum(mort_life_x$a)
# x_vec <- x_a[4:(x_length - 1)] - 21
# x_vec2 <- c(x_vec[-1], 99)
# x_mid <- (x_vec + x_vec2) / 2
# Dx_vec <- mort_life_x$Dx[5:x_length]
# qx_vec <- mort_life_x$qx[5:x_length]
# lx_vec <- mort_life_x$lx[5:x_length]/100
# mort_df_x <- data.frame(x_vec,  x_vec2, Dx_vec, qx_vec, lx_vec)
# 
# nls_fit <- nls(lx_vec ~ exp(a/b - a/b * exp(b * x_vec ) ) , 
#                data = mort_df_x, start=list(a = 0.001, b = 0.075), weights = Dx_vec)#, method="Nelder-Mead")
# NLS_Gompertz_shape <- summary(nls_fit)$coefficients[2]
# NLS_Gompertz_rate <- summary(nls_fit)$coefficients[1]

# data from Wendler 2008, 256 Tab. 3.9, Superintendentur Uelzen, including city of Uelzen, 1755--1777
uelzen <- data.frame(Age = c(20, 30, 40, 50, 60, 70, 80, 90, 100), Dx = c(442, 604, 687, 892, 1069, 691, 247, 25, 6) )
uelzen$death <- 1
uelzen_surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 20, death) ~ 1, 
                                            data = uelzen, dist="gompertz", weights = Dx)
uelzen_surv_lt_Gompertz_shape <- uelzen_surv_lt$coefficients[1]
uelzen_surv_lt_Gompertz_rate <- exp(uelzen_surv_lt$coefficients[2])
uelzen_result <- data.frame(names = "Uelzen", group = "historic", year = 1766, beta = uelzen_surv_lt_Gompertz_shape, alpha = uelzen_surv_lt_Gompertz_rate)


# # Süßmilch, after https://books.google.de/books?id=txMlBgAAQBAJ&pg=PA751&lpg=PA751&dq=Süßmilch+demographie+sterbetafel&source=bl&ots=B2X0Y5K5Zl&sig=ACfU3U3nv6LJhmU_cvo6EpnvUZVqOzHdyA&hl=de&sa=X&ved=2ahUKEwizsIHavc73AhXSjKQKHfrHAIIQ6AF6BAgUEAM#v=onepage&q=Süßmilch%20demographie%20sterbetafel&f=false
# # S. 752; die resultierende Sterbetafel ist allerdings unrealistisch
# suessmilch <- data.frame(Age = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80), 
# qx = c(1.43, 1.67, 2.00, 2.27, 2.86, 3.33, 4.00, 5.00, 6.67, 10.00, 14.29, 22.22) )
# # calculation of lx
# lx_c <- NULL
# lx <- 1
# for (k in 2:length(suessmilch$Age)) {
#   lx_1 <- lx * (1 - suessmilch$qx[k-1]* 0.01)
#   lx_c <- c(lx_c, lx_1)
#   lx <- lx_1
# }
# suessmilch$lx <- c(1, lx_c)
# suessmilch$Age_mid <- suessmilch$Age - 22.5
# 
# suessmilch_nls_fit <- nls(lx ~ exp(a/b - a/b * exp(b * (Age - 25) ) ) , 
#                      data = suessmilch, start=list(a = 0.001, b = 0.075) )
# suessmilch_NLS_Gompertz_shape <- summary(suessmilch_nls_fit)$coefficients[2]
# suessmilch_NLS_Gompertz_rate <- summary(suessmilch_nls_fit)$coefficients[1]
# 
# mort_fit_WOLS_estim <- lm(log(qx*0.001)  ~ Age_mid, data = suessmilch)
# WOLS_estim_Gompertz_shape <- mort_fit_WOLS_estim$coefficients[2]
# WOLS_estim_Gompertz_rate <- exp(mort_fit_WOLS_estim$coefficients[1])
# 
# 
# ggplot() + xlim(20, 100) + ylab("density") + ylim(0, 0.4) +
#   geom_function(fun = function(x) flexsurv::hgompertz(x - 25, suessmilch_NLS_Gompertz_shape, suessmilch_NLS_Gompertz_rate), colour = "red") +
#   geom_function(fun = function(x) flexsurv::hgompertz(x - 20, uelzen_surv_lt_Gompertz_shape,  uelzen_surv_lt_Gompertz_rate), colour= "blue") +
#   geom_function(fun = function(x) flexsurv::hgompertz(x - 21, radolfzell_surv_lt_Gompertz_shape,  radolfzell_surv_lt_Gompertz_rate)) +
#   geom_function(fun = function(x) flexsurv::hgompertz(x - 21, NLS_Gompertz_shape,  NLS_Gompertz_rate)) +
#   xlab("Gompertz distribution")
# 
# ggplot() + xlim(20, 105) + ylab("density") + ylim(0, 1) +
#   geom_function(fun = function(x) gomp_lx(x - 25, suessmilch_NLS_Gompertz_rate, suessmilch_NLS_Gompertz_shape), colour = "red") +
#   geom_function(fun = function(x) gomp_lx(x - 20,  uelzen_surv_lt_Gompertz_rate, uelzen_surv_lt_Gompertz_shape), colour= "blue") +
#   geom_function(fun = function(x) gomp_lx(x - 21,  radolfzell_surv_lt_Gompertz_rate, radolfzell_surv_lt_Gompertz_shape)) +
#   geom_function(fun = function(x) gomp_lx(x - 21, NLS_Gompertz_rate, NLS_Gompertz_shape)) +
#   xlab("Gompertz distribution")
# 
# ggplot() + xlim(25, 105) + ylab("density") +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 25, suessmilch_NLS_Gompertz_shape, suessmilch_NLS_Gompertz_rate), colour = "red") +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 20, uelzen_surv_lt_Gompertz_shape,  uelzen_surv_lt_Gompertz_rate), colour= "blue") +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 21, radolfzell_surv_lt_Gompertz_shape,  radolfzell_surv_lt_Gompertz_rate)) +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 21, NLS_Gompertz_shape,  NLS_Gompertz_rate)) +
#   xlab("Gompertz distribution")
