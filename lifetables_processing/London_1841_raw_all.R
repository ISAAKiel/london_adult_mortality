# first reliable census data for London from 1841
if (runCodeNew){
  set.seed(7355)
  year_data <- read.table("./data/London_1841_raw.txt", header=TRUE, sep = "\t")
  # year_data <- read.table("./data/Lonond_1841_raw_full.txt", header=TRUE, sep = "\t")
  year_data$pop <- year_data$pop_male + year_data$pop_female
  year_data$deaths <- year_data$death_male + year_data$death_female
  year_data_red <- year_data %>% group_by(group) %>% summarize(age = min(age), 
                                                               pop = sum(pop),
                                                               deaths = sum(deaths)) %>% data.frame(.)
  for(i in 1:8) {
    if(i < 8) {
      year_data_red$n[i] <- year_data_red$age[i + 1] - year_data_red$age[i]
      year_data_red$nax[i] <- (year_data_red$age[i + 1] - year_data_red$age[i]) / 2
    } else{
      year_data_red$n[i] <- 10
      year_data_red$nax[i] <- year_data_red$pop[i] / year_data_red$deaths[i] # mx for open-ended last interval
    }
  }
  
  year_data_red$mx <- year_data_red$deaths / year_data_red$pop
  df_length <- length(year_data_red$age)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  qx <- NULL
  for (k in 1: df_length ) {
    lx_ <- c(lx_, lx)
    mx <- year_data_red$mx[k]
    nax <- year_data_red$nax[k]
    n_ <- year_data_red$n[k]
    if(k < 8){
      qx_1 <- n_ * mx / (1 + nax * mx)
    } else {
      qx_1 <- 1
    }
    lx_old <- lx
    lx <- (1 - qx_1) * lx_old
    dx_1 <- lx_old - lx
    dx <- c(dx, dx_1)
    qx <- c(qx, qx_1)
  }
  year_data_red$lx <- lx_
  year_data_red$qx <- qx
  year_data_red$dx <- dx
  
  year_data_uncount <- year_data_red %>% uncount(round(dx * 1000))
  year_data_uncount$age_end <- year_data_uncount$age + 5
  
  gomp.anthr_age(year_data_uncount, age_beg = "age", age_end = "age_end",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 300000,
                 minimum_age = 15) %>%
    diagnostic.summary(., HDImass = 0.95) -> London_1841_result
  # saves results in Rda-object
  save(London_1841_result, file = file.path(".", saveFileDir, "London_1841_result.Rda") )
}
load(file.path(".", saveFileDir, "London_1841_result.Rda") )

# mode values
modes <- London_1841_result[2:3,]$Mode

# range of Gompertz beta values
beta_range <- paste0(format(round(London_1841_result[2,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                     format(round(London_1841_result[2,]$HDIhigh, digits = 4), nsmall = 4  ) )
# range of age modes M
M_range <- paste0(format(round(London_1841_result[3,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                  format(round(London_1841_result[3,]$HDIhigh, digits = 1), nsmall = 1 ) )

London_1841_ranges <- data.frame(parameter = c("beta", "M"), modes = format(round(modes, 4), nsmall = 4), HDI.ranges = c(beta_range, M_range))