if (runCodeNew){
  set.seed(3932)
  year_data <- read.table("./data/London_1841.txt", header=TRUE, sep = "\t")
  year_data$mx <- year_data$mortality_per_cent / 100
  df_length <- length(year_data$age)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  qx <- NULL
  for (k in 1: df_length ) {
    mx <- year_data$mx[k]
    qx_1 <- 5 * mx / (1 + 2.5 * mx)
    dx_1 <- qx_1 * lx
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
    qx <- c(qx, qx_1)
  }
  year_data$dx <- dx
  year_data$qx <- qx
  
  year_data_uncount <- year_data %>% uncount(round(dx * 1000))
  year_data_uncount$age_end <- year_data_uncount$age + 5
  bayes_anthr_gomp_b <- NA
  bayes_anthr_gomp_a <- NA
  tryCatch({
    gomp.anthr_age(year_data_uncount, age_beg = "age", age_end = "age_end",
                   silent.jags = FALSE,
                   silent.runjags = FALSE,
                   thinSteps = 1,
                   numSavedSteps = 300000,
                   minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  }, error=function(e){})
  
  London_1841_result <- gomp_anthr_MCMC_diag[1:2,]
  
  # saves results in Rda-object
  save(London_1841_result, file = file.path(".", saveFileDir, "London_1841_result.Rda") )
}
load(file.path(".", saveFileDir, "London_1841_result.Rda") )