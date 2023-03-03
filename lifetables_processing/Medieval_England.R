# Data on English monks, peers and tenants from Hatcher 1986, 38 Table 5
# only on Christ Church monks: 28 Table 2
if (runCodeNew){
  set.seed(847)
  #medieval <- read.table("./data/Hatcher_monks.txt", header=TRUE, sep = "\t")
  #medieval$Tenants <- (medieval$Tenants_I + medieval$Tenants_II) / 2
  monks <- read.table("./data/Christ_church_monks.txt", header=TRUE, sep = "\t", skip = 1)
  monks_n <- monks[1,-1]
  monks <- monks[-1,]
  #group_data <- medieval[,1:2]
  #colnames(group_data) <- c("Age", "qx")
  monks_melt <- na.omit(reshape2::melt(monks, id.vars = "Age", value.name = "qx"))
  
  group <- unique(monks_melt$variable)
  monks_result <- data.frame()
  for(i in group) {
   group_data <- monks_melt[ which(monks_melt$variable == i), ]
  
  df_length <- length(group_data$Age)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- group_data$qx[k] * lx / 1000
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
  }
  group_data$dx <- dx
  group_data$Dx <-  round(group_data$dx * monks_n[1,i]) # n according to Hatcher
  
  max_Age <- max(group_data$Age)
  # Bayes
  year_data_uncount <- group_data %>% uncount(Dx)
  year_data_uncount$age_end <- ifelse(year_data_uncount$Age < max_Age, year_data_uncount$Age + 5, year_data_uncount$Age + 100 - max_Age)
  
  gomp.anthr_age(year_data_uncount, age_beg = "Age", age_end = "age_end",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 200000,
                 minimum_age = 20) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  
  ind_result <- cbind(  start = as.numeric(substr(i, 2, 5)), end = as.numeric(substr(i, 7, 10)), 
                       parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
  rownames(ind_result) <- NULL
  monks_result <- rbind(monks_result, ind_result )
  }
  # saves results in Rda-object
  save(monks_result, file = file.path(".", saveFileDir, "monks_result.Rda") )
}
load(file.path(".", saveFileDir, "monks_result.Rda") )
# mode values
# gomp.ex() s. helper_functions.R
modes <- monks_result[2:3,]$Mode
monks_result <-  monks_result[1:3,]
monks_20ex <- gomp.ex(20, monks_result[1,8], monks_result[2,8], age_start = 20)
monks_25ex <- gomp.ex(25, monks_result[1,8], monks_result[2,8], age_start = 20)

# range of Gompertz beta values
beta_range <- paste0(format(round(monks_result[2,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                     format(round(monks_result[2,]$HDIhigh, digits = 4), nsmall = 4  ) )
# range of age modes M
M_range <- paste0(format(round(monks_result[3,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                  format(round(monks_result[3,]$HDIhigh, digits = 1), nsmall = 1 ) )

monks_ranges <- data.frame(parameter = c("beta", "M", "e20", "e25"), 
                           modes = format(round(c(modes, monks_20ex, monks_25ex), 4), nsmall = 4), 
                           HDI.ranges = c(beta_range, M_range, NA, NA))

monks_prep <- data.frame(source = "written", data = "Christ Church monks", M = monks_result[3,8], start = 1395, end = 1505, 
                         year = NA, monks_result[3,c(12, 13)])
monks_beta_prep <- data.frame(source = "written", data = "Christ Church monks", beta = monks_result[2,8], start = 1395, end = 1505, 
                         year = NA, monks_result[2,c(12, 13)])
