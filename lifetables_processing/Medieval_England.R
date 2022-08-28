# Data on English monks, peers and tenants from Hatcher 1986, 38 Table 5
if (runCodeNew){
  set.seed(847)
  medieval <- read.table("./data/Hatcher_monks.txt", header=TRUE, sep = "\t")
  medieval$Tenants <- (medieval$Tenants_I + medieval$Tenants_II) / 2
  monks <- medieval[,1:2]
  colnames(monks) <- c("age", "qx")
  #medieval_melt <- reshape2::melt(medieval, id.vars = "Age", value.name = "qx")
  
  # group <- unique(medieval_melt$variable)
  
  # for(i in group) {
  #   group_data <- medieval_melt[ which(medieval_melt$variable == i), ]
  
  df_length <- length(monks$age)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- monks$qx[k] * lx / 1000
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
  }
  monks$dx <- dx
  monks$Dx <-  round(monks$dx * 395) # n according to Hatcher
  
  # group_data$death <- 1
  # sample_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age_mod, death) ~ 1, 
  #                                         data = group_data, dist="gompertz", weights = dx)
  # sample_data_lt_Gompertz_shape <- sample_data_lt$coefficients[1]
  # sample_data_lt_Gompertz_rate <- exp(sample_data_lt$coefficients[2])
  # 
  # ind_result <- cbind(group = i, beta = sample_data_lt_Gompertz_shape, alpha = sample_data_lt_Gompertz_rate)
  
  # Bayes
  year_data_uncount <- monks %>% uncount(Dx)
  year_data_uncount$age_end <- ifelse(year_data_uncount$age < 80, year_data_uncount$age + 5, year_data_uncount$age + 20)
  
  gomp.anthr_age(year_data_uncount, age_beg = "age", age_end = "age_end",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 200000,
                 minimum_age = 20) %>%
    diagnostic.summary(., HDImass = 0.95) -> monks_result
  
  # saves results in Rda-object
  save(monks_result, file = file.path(".", saveFileDir, "monks_result.Rda") )
}
load(file.path(".", saveFileDir, "monks_result.Rda") )
# mode values
modes <- monks_result[2:3,]$Mode

# range of Gompertz beta values
beta_range <- paste0(format(round(monks_result[2,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                     format(round(monks_result[2,]$HDIhigh, digits = 4), nsmall = 4  ) )
# range of age modes M
M_range <- paste0(format(round(monks_result[3,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                  format(round(monks_result[3,]$HDIhigh, digits = 1), nsmall = 1 ) )

monks_ranges <- data.frame(parameter = c("beta", "M"), modes = format(round(modes, 4), nsmall = 4), HDI.ranges = c(beta_range, M_range))