#medieval data, Russell, from https://genus.springeropen.com/articles/10.1186/s41118-021-00122-w/tables/2
if (runCodeNew){
  set.seed(167)
  path <- "./data/russell.txt"
  peers <- read.table(path, header=TRUE, sep = "\t")
  peers$heirs_cumsum <- cumsum(peers$heirs)
  peers$death_cumsum <- c(0, cumsum(peers$deaths)[-21])
  peers$risk <- peers$heirs_cumsum - peers$death_cumsum
  peers$mx <- peers$deaths / peers$risk
  
  peers_subset <- subset(peers, age >= 15)
  
  peers_subset$Lx <- peers_subset$risk * 5 - peers_subset$deaths * 2.5 - peers_subset$heirs * 2.5
  peers_subset$dx <- peers_subset$Lx * peers_subset$mx /1000
  peers_subset$dx <- peers_subset$dx / sum(peers_subset$dx)
  peers_subset$age2 <- peers_subset$age + 5
  
  df_length <- length(peers_subset$age)
  lx_ <- NULL
  lx <- 1
  for (k in 2: df_length ) {
    lx_ <- c(lx_, lx)
    lx <- lx - peers_subset$dx[k-1]
  }
  
  peers_subset$lx <- c(lx_, peers_subset$dx[df_length])
  peers_subset$qx <- peers_subset$dx / peers_subset$lx
  peers_subset$Dx <-  round(peers_subset$dx * 532) # n according to Russell
  year_data_uncount <- peers_subset %>% uncount(Dx)
  
  gomp.anthr_age(year_data_uncount, age_beg = "age", age_end = "age2",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 200000,
                 minimum_age = 15) %>%
    diagnostic.summary(., HDImass = 0.95) -> Peers_result

# saves results in Rda-object
save(Peers_result, file = file.path(".", saveFileDir, "Peers_result.Rda") )
}
load(file.path(".", saveFileDir, "Peers_result.Rda") )

modes <- Peers_result[2:3,]$Mode
Peers_20ex <- gomp.ex(20, Peers_result[1,5], Peers_result[2,5])
Peers_25ex <- gomp.ex(25, Peers_result[1,5], Peers_result[2,5])

# range of Gompertz beta values
beta_range <- paste0(format(round(Peers_result[2,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                     format(round(Peers_result[2,]$HDIhigh, digits = 4), nsmall = 4  ) )
# range of age modes M
M_range <- paste0(format(round(Peers_result[3,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                  format(round(Peers_result[3,]$HDIhigh, digits = 1), nsmall = 1 ) )

Peers_ranges <- data.frame(parameter = c("beta", "M", "e20", "e25"), 
                           modes = format(round(c(modes, Peers_20ex, Peers_25ex), 4), nsmall = 4), 
                           HDI.ranges = c(beta_range, M_range, NA, NA))

Peers_prep <- data.frame(source = "written", data = "English Peers", M = Peers_result[3,5], start = 1275, end = 1300, 
                         year = NA, Peers_result[3,c(9, 10)])