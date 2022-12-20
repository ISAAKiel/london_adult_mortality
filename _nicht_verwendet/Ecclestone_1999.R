#medieval data, from Ecclestone 1999
# irgendwo ist in den Berechnung von Ecclestone der Wurm drin:
# für den Rückgang an gelebten Jahren ist die Zahl der Toten viel zu
# zu gering. Alleine von Zeile 1 zu Zeile 2 beträgt der Rückgang fast
# 200 Personenjahre, d. h. 40 Personen, gestorben sind aber angeblich nur
# 9. Wo sind die alle abgeblieben? Ach so, die sind vermutlich andernweitig
# verschwunden, zu den Persons at risk zählten sie aber bis zum Ausscheiden
# trotzdem dazu.
# Trotzdem ist die Rate ab 40 Jahren viel zu niedrig, s. Plot.

#if (runCodeNew){
  set.seed(8928)
  path <- "./data/Ecclestone_1999.txt"
  path <- "./data/Ecclestone_1999_short.txt"
  garciones <- read.table(path, header=TRUE, sep = "\t")
  garciones$age <- garciones$age + 14 # age starts at 14
  garciones$age2 <- garciones$age2 + 14 # age starts at 14
  garciones$mx <- garciones$deaths / garciones$Lx
  garciones$qx <- 5 * garciones$mx / (1 + 2.5 * garciones$mx )
  
  ggplot(garciones) + geom_point(aes(x = age, y = qx))
  
  # garciones$lx <- (garciones$Lx - (garciones$deaths * 
  #                                    (garciones$age2 - garciones$age + 1) / 2 ) ) / 
  #                    (garciones$age2 - garciones$age + 1)
  
  df_length <- length(garciones$age)
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- garciones$qx[k] * lx
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
  }
  garciones$dx <- dx
  garciones$lx <- c(1, lx_[-df_length])
  
  garciones$lx <- c(lx_, garciones$dx[df_length])
  garciones$qx <- garciones$dx / garciones$lx
  garciones$Dx <-  round(garciones$dx * 46) # n according to Russell
  year_data_uncount <- garciones %>% uncount(deaths)
  
  gomp.anthr_age(year_data_uncount, age_beg = "age", age_end = "age2",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 200000,
                 minimum_age = 14) %>%
    diagnostic.summary(., HDImass = 0.95) -> garciones_result
  
  # saves results in Rda-object
  save(peers_result, file = file.path(".", saveFileDir, "peers_result.Rda") )
}
load(file.path(".", saveFileDir, "peers_result.Rda") )

modes <- peers_result[2:3,]$Mode

# range of Gompertz beta values
beta_range <- paste0(format(round(peers_result[2,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                     format(round(peers_result[2,]$HDIhigh, digits = 4), nsmall = 4  ) )
# range of age modes M
M_range <- paste0(format(round(peers_result[3,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                  format(round(peers_result[3,]$HDIhigh, digits = 1), nsmall = 1 ) )

peers_ranges <- data.frame(parameter = c("beta", "M"), modes = format(round(modes, 4), nsmall = 4), HDI.ranges = c(beta_range, M_range))

peers_prep <- data.frame(source = "written", data = "English Peers", M = peers_result[3,5], start = 1275, end = 1300, 
                         year = NA, peers_result[3,c(9, 10)])