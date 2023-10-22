# English data, from Wrigley et al. 1997, 290 Tab. 6.19 
# Adult mortality, sexes combined (1000qx), 1640-1809, for age groups 25-84 years
if (runCodeNew){
  set.seed(9871)
eng_mort <- read.table("./data/wrigley_et_al_1997_england_1640-1809.txt", header=TRUE, sep = "\t", skip = 1)

eng_mort_melt <- reshape2::melt(eng_mort, id.vars = "Age", value.name = "qx")

years <- unique(eng_mort_melt$variable)

eng_mort_result <- data.frame()
for(i in years) {
  year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
  year_data$age_mod <- year_data$Age - 25
  
  df_length <- length(year_data$age_mod)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- year_data$qx[k] * lx / 1000
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
  }
  year_data$dx <- dx
  
  year_data_uncount <- year_data %>% uncount(round(dx * 1000))
  year_data_uncount$age_end <- ifelse(year_data_uncount$Age < 80, year_data_uncount$Age + 5, year_data_uncount$Age + 20)
  bayes_anthr_gomp_b <- NA
  bayes_anthr_gomp_a <- NA
  tryCatch({
    gomp.anthr_age(year_data_uncount, age_beg = "Age", age_end = "age_end",
                   silent.jags = TRUE,
                   silent.runjags = TRUE,
                   thinSteps = 1,
                   numSavedSteps = 300000,
                   minimum_age = 25) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  }, error=function(e){})
  
  ind_result <- cbind(year = i, parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
  rownames(ind_result) <- NULL
  eng_mort_result <- rbind(eng_mort_result, ind_result )
}
# saves results in Rda-object
save(eng_mort_result, file = file.path(".", saveFileDir, "eng_mort_result.Rda") )
}
load(file.path(".", saveFileDir, "eng_mort_result.Rda") )

# range of Gompertz beta values
beta_range <- paste0(round(min(eng_mort_result[which(eng_mort_result$parameter == "beta"),]$Mode), 4), "-",
                     round(max(eng_mort_result[which(eng_mort_result$parameter == "beta"),]$Mode) , 4) )
# range of age modes M
M_range <- paste0(round(min(eng_mort_result[which(eng_mort_result$parameter == "M"),]$Mode), 1), "-",
                  round(max(eng_mort_result[which(eng_mort_result$parameter == "M"),]$Mode), 1) )

eng_mort_ranges <- data.frame(parameter = c("beta", "M"), ranges = c(beta_range, M_range))