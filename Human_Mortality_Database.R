HMD_username <- readline(prompt = "Enter username: ")
HMD_password <- readline(prompt="Enter password: ")
credentials <- c(HMD_username, HMD_password)

set.seed(9989)
random_numb <- round(runif(n = 1, min = 1, max = 232))
start_numb <- 1
HDM_countries <- c("SWE", "CHE", "DNK", "FRATNP", "GBRTENW", "NLD")
lt_result <- data.frame()
for(t in HDM_countries) {
  # this extracts life table data for both sexes
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


# comparing Gompertz beta values for Dx and dx
swe_dx <- HMDHFDplus::readHMDweb("SWE", "bltper_5x5", credentials[1], credentials[2])
swe_Dx <- HMDHFDplus::readHMDweb("SWE", "Deaths_5x5", credentials[1], credentials[2])
swe_pop <- HMDHFDplus::readHMDweb("SWE", "Population5", credentials[1], credentials[2])

lt_result_dx <- data.frame()
years <- unique(swe_dx$Year)
for(i in years) {
  year_data <- swe_dx[ which(swe_dx$Year == i), ]
  year_data <- subset(year_data, Age >= 15)
  year_data$death <- 1 
  surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 15, death) ~ 1, data = year_data, dist="gompertz", weights = dx)
  surv_lt_Gompertz_shape <- surv_lt$coefficients[1]
  surv_lt_Gompertz_rate <- exp(surv_lt$coefficients[2])
  ind_result <- cbind(year = i, beta = surv_lt_Gompertz_shape, alpha = surv_lt_Gompertz_rate)
  lt_result_dx <- rbind(lt_result_dx, ind_result )
}
rownames(lt_result_dx) <- NULL
cols.num <- c("year", "beta", "alpha")
lt_result_dx[cols.num] <- sapply(lt_result_dx[cols.num],as.numeric)

lt_result_Dx <- data.frame()
years <- unique(swe_Dx$Year)
for(i in years) {
  year_data <- swe_Dx[ which(swe_Dx$Year == i), ]
  year_data <- subset(year_data, Age >= 15)
  year_data$death <- 1 
  surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 15, death) ~ 1, data = year_data, dist="gompertz", weights = Total)
  surv_lt_Gompertz_shape <- surv_lt$coefficients[1]
  surv_lt_Gompertz_rate <- exp(surv_lt$coefficients[2])
  ind_result <- cbind(year = i, beta = surv_lt_Gompertz_shape, alpha = surv_lt_Gompertz_rate)
  lt_result_Dx <- rbind(lt_result_Dx, ind_result )
}
rownames(lt_result_Dx) <- NULL
cols.num <- c("year", "beta", "alpha")
lt_result_Dx[cols.num] <- sapply(lt_result_Dx[cols.num],as.numeric)

lt_result_pop <- data.frame()
years <- unique(swe_Dx$Year)
for(i in years) {
  year_data <- swe_pop[ which(swe_pop$Year == i), ]
  pop_sum <- sum(year_data$Total2)
  pop_result <- c(year = i, pop = pop_sum)
  lt_result_pop <- rbind(lt_result_pop, pop_result )
}
colnames(lt_result_pop) <- c("year", "pop")
lt_result_pop$diff <- c(0, lt_result_pop$pop[-1] - lt_result_pop$pop[-55]) / lt_result_pop$pop

swe_e15 <- swe_dx[which(swe_dx$Age == 15),]
colnames(swe_e15) <- c("year", "age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex","OpenInterval")

merge_1 <- merge(lt_result_Dx, lt_result_dx, by="year")
merge_2 <- merge(merge_1, lt_result_pop, by="year")
merge_swe <- merge(merge_2, swe_e15, by="year")

# swe_list <- list() 
# swe_list[[1]] <- ggplot(merge_swe) + geom_line(aes(x = year, y = beta.x, colour = "red")) + 
#   geom_line(aes(x = year, y = beta.y, colour = "black"))  + ylab("Gompertz \u03B2") +
#   #labs(color = "parameter") + 
#   theme(legend.position = c(0.125, 0.8)) +
#   scale_color_manual(labels = c(expression(d[x]), expression(D[x])), values = c("black", "red"))
# swe_list[[2]] <- ggplot(merge_swe, aes(x = (beta.y - beta.x)/beta.x*100)) + 
#   geom_density(bw = 2) + xlab("Difference in Gompertz \u03B2 in %")
# swe_list[[3]] <- ggplot(merge_swe, aes(x = year, y = (beta.y - beta.x)/beta.x*100)) + 
#   geom_point() + ylab("Difference in Gompertz \u03B2 in %")
# swe_list[[4]] <- ggplot(merge_swe, aes(x = diff * 100, y = (beta.y - beta.x)/beta.x*100)) + 
#   geom_point() + geom_smooth(method='lm', formula= y~x) + ylab("difference in Gompertz \u03B2 in %") +
#   xlab("population increase in %")

swe_list <- list() 
swe_list[[1]] <- ggplot(merge_swe) + geom_line(aes(x = year, y = beta.x, colour = "red")) + 
  geom_line(aes(x = year, y = beta.y, colour = "black"))  + ylab("Gompertz \u03B2") +
  #labs(color = "parameter") + 
  theme(legend.position = c(0.125, 0.8)) +
  scale_color_manual(labels = c(expression(d[x]), expression(D[x])), values = c("black", "red"))
swe_list[[2]] <- ggplot(merge_swe, aes(x = (beta.y - beta.x)) ) + 
  geom_density(bw = 0.001) + xlab("Difference in Gompertz \u03B2")
swe_list[[3]] <- ggplot(merge_swe, aes(x = year, y = (beta.y - beta.x)) ) + 
  geom_point() + ylab("Difference in Gompertz \u03B2")
swe_list[[4]] <- ggplot(merge_swe, aes(x = diff * 100, y = (beta.y - beta.x))) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) + ylab("difference in Gompertz \u03B2") +
  xlab("population increase in %")

 fit <- lm((beta.x - beta.y) ~ diff, data = merge_swe) #subset(merge_swe, year !=1825 & year !=1775) ) 
 summary(fit)

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
