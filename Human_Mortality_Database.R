HMD_username <- readline(prompt = "Enter username: ")
HMD_password <- readline(prompt="Enter password: ")
credentials <- c(HMD_username, HMD_password)

set.seed(9989)
random_numb <- round(runif(n = 1, min = 1, max = 232))
start_numb <- 1
HDM_countries <- c("SWE", "CHE", "DNK", "FRATNP", "GBRTENW", "NLD")
lt_result <- data.frame()
for(t in HDM_countries) {
  country_lt <- HMDHFDplus::readHMDweb(t, "bltper_5x5", credentials[1], credentials[2])
  years <- unique(country_lt$Year)
  for(i in years) {
    year_data <- country_lt[ which(country_lt$Year == i), ]
    year_data <- subset(year_data, Age >= 15)
    year_data$death <- 1 
    surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 15, death) ~ 1, data = year_data, dist="gompertz", weights = dx)
    surv_lt_Gompertz_shape <- surv_lt$coefficients[1]
    surv_lt_Gompertz_rate <- exp(surv_lt$coefficients[2])
    ind_result <- cbind(country = t, year = i, beta = surv_lt_Gompertz_shape, alpha = surv_lt_Gompertz_rate)
    lt_result <- rbind(lt_result, ind_result )
    if(start_numb == random_numb){
      random_data <- year_data
      random_Gompertz_shape <- surv_lt_Gompertz_shape
      random_Gompertz_rate <- surv_lt_Gompertz_rate
    }
    start_numb <- start_numb + 1
  }
}
rownames(lt_result) <- NULL
cols.num <- c("year", "beta", "alpha")
lt_result[cols.num] <- sapply(lt_result[cols.num],as.numeric)

HMD_plot <- ggplot(lt_result, aes(x = year, y = beta)) + geom_point(aes(colour = country)) + ylab("Gompertz \u03B2") +
  geom_smooth(method='loess', span = 0.25, formula = y ~ x, colour = "red", se = TRUE, level = 0.95)

# plot_list <- list()
# for (j in HDM_countries) {
#   country_data <- lt_result[ which(lt_result$country == j), ]
#   plot_list[[j]] <- ggplot(country_data, aes(x = year, y = beta)) + geom_point(colour = "grey") + ylab("Gompertz \u03B2") +
#     geom_smooth(method='loess', span = 0.25, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) + 
#     xlab(j)
# }
# 
# do.call(gridExtra::grid.arrange, plot_list)
# 
# # extract single life table for
# ggplot(random_data) + geom_point(aes(x = Age, y = log(mx))) + xlim(15, 105) + ylab("hazard") +
#   geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, random_Gompertz_shape,random_Gompertz_rate )), colour = "red")
# 
# ggplot(random_data) + geom_point(aes(x = Age, y = lx/100000)) + xlim(15, 105) + ylab("density") +
#   geom_function(fun = function(x) gomp_lx(x - 15,random_Gompertz_rate, random_Gompertz_shape), colour = "red")
