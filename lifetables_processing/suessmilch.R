# Süßmilch
# https://books.google.de/books?id=y-M_AAAAcAAJ&pg=RA1-PA47&lpg=RA1-PA47&dq=süßmilch+tabula+XIII&source=bl&ots=hCp3gxPbWF&sig=ACfU3U1HJFGsxRpcQdfE9c563Ngz6FEULQ&hl=de&sa=X&ved=2ahUKEwj04rOj5c_3AhWGlqQKHb1xCmkQ6AF6BAgZEAM#v=onepage&q=süßmilch%20tabula%20XIII&f=false
# Anhang S. 31 Tab. 10: Gestorbene in London, 1728-1757
# Anhang S. 34 Tab. 12: S. Sulpice in Paris, ca. 1750 ("letzte 30 Jahre")
# Anhang S. 37 Tab. 13:Berlin 1752-1755
# Anhang S. 38 Tab. 14: St. Petri/Berlin (1727-1750), Braunschweig (1746-1750), Breslau (1722-1724)
# Anhang S. 40 Tab. 15: Französische Kolonie in Berlin, 1746-1755, Berlin 1746
# Anhang S. 42 Tab. 17: Fürstenwalde, 1732-1760

London <- data.frame(Age = c( 20, 30, 40, 50, 60, 70, 80, 90), Dx = c( 58474, 71502, 73238, 59782, 47269, 33679, 16948, 2979) )
London$death <- 1
London_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 20, death) ~ 1, 
                                        data = London, dist="gompertz", weights = Dx)
London_lt_Gompertz_shape <- London_lt$coefficients[1]
London_lt_Gompertz_rate <- exp(London_lt$coefficients[2])
London_result <- data.frame(names = "London", group = "Süßmilch", year = 1742, beta = London_lt_Gompertz_shape, alpha = London_lt_Gompertz_rate)

Paris <- data.frame( Age = c( 21, 31, 46, 61, 71, 81, 91, 96) , Dx = c( 2630, 4522, 5068, 3945, 3505, 1673, 101, 72) )
Paris$death <- 1
Paris_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 21, death) ~ 1, 
                                   data = Paris, dist="gompertz", weights = Dx)
Paris_lt_Gompertz_shape <- Paris_lt$coefficients[1]
Paris_lt_Gompertz_rate <- exp(Paris_lt$coefficients[2])
Paris_result <- data.frame(names = "Paris", group = "Süßmilch", year = 1750, beta = Paris_lt_Gompertz_shape, alpha = Paris_lt_Gompertz_rate)

suessmilch_data <- read.table("./data/Suessmilch_data.txt", header = TRUE, sep = "\t")

suessmilch_melt <- reshape2::melt(suessmilch_data, id.vars = c("Age", "group"), value.name = "Dx")

# the data has to aggregated by decades as it is very apparent that often the age was not exactly known.
suessmilch_melt_df <- suessmilch_melt %>% group_by(variable, group) %>% summarize(Dx = mean(Dx)) %>% data.frame(.)

cities <- unique(suessmilch_melt_df$variable)

suessmilch_result <- data.frame()
for(i in cities) {
  cities_data <- suessmilch_melt_df[ which(suessmilch_melt_df$variable == i), ]
  cities_data$Age <- c(16, 21, 31, 41, 51, 61, 71, 81, 91, 101)
  
  cities_data$death <- 1
  cities_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 16, death) ~ 1, 
                                              data = cities_data, dist="gompertz", weights = Dx)
  cities_data_lt_Gompertz_shape <- cities_data_lt$coefficients[1]
  cities_data_lt_Gompertz_rate <- exp(cities_data_lt$coefficients[2])

  
  ind_result <- cbind(names = i, group = "Süßmilch", year = substring(i, nchar(i)- 3, nchar(i)), 
                      beta = cities_data_lt_Gompertz_shape, alpha = cities_data_lt_Gompertz_rate)
  suessmilch_result <- rbind(suessmilch_result, ind_result )
}
rownames(suessmilch_result) <- NULL
cols.num <- c("beta", "alpha")
suessmilch_result[cols.num] <- sapply(suessmilch_result[cols.num],as.numeric)


# ggplot() + xlim(16, 105) + ylab("density") +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 16, suessmilch_result$beta[1], suessmilch_result$alpha[1]), colour = "red") +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 16, suessmilch_result$beta[2], suessmilch_result$alpha[2]), colour= "blue") +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 16, suessmilch_result$beta[3], suessmilch_result$alpha[3]), colour= "grey") +
#   geom_function(fun = function(x) flexsurv::dgompertz(x - 16, suessmilch_result$beta[4], suessmilch_result$alpha[4])) +
#   xlab("Gompertz distribution")
