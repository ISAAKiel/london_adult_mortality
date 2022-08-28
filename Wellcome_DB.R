molas_cemeteries <- c("Bermondsey Abbey", "Merton Priory", "St. Mary Graces", "St. Benet Sherehog", "St. Marylebone", "St. Bride's lower churchyard")

if (runCodeNew){
  set.seed(982)
  age_beg <- c(12, 18, 26, 36, 46, 18)
  age_end <- c(18, 26, 36, 46, 100, 100)
  #guildhall <- c(4, 7, 10, 11, 6, 13)
  bermondsey_abbey <- c(1, 15, 28, 51, 37, 69)
  merton_priory <- c(19, 27, 102, 262, 84, 168)
  st_mary <- c(25, 32, 58, 69, 40 ,84)
  st_benet <- c(12, 9, 33, 50, 32, 43)
  st_marylebone <- c(3, 20, 42, 69, 52, 40)
  st_brides_lower <- c(10, 10, 44, 88, 162, 65)
  molas_dating_start <- c(1089, 1117, 1350, 1500, 1742, 1770)
  molas_dating_end <- c(1538, 1538, 1540, 1699, 1817, 1849)
  wellcome_data <- data.frame(age_beg, age_end, #guildhall, 
                              bermondsey_abbey, merton_priory, st_mary, st_benet, st_marylebone, st_brides_lower)
  length(wellcome_data)
  wellcome_names <- colnames(wellcome_data)
  
  wellcome_result <- data.frame()
  for (t in 1:(length(wellcome_data) - 2) ) {
    molas_ind <- data.frame(wellcome_data[,1:2], dx = wellcome_data[,(t + 2)])
    year_data_uncount <- molas_ind %>% uncount(dx)
    
    #Bayesian modell with anthropological age estimate
    gomp.anthr_age(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                   thinSteps = 1, minimum_age = 12,
                   numSavedSteps = 400000) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind( cemetery = wellcome_names[t + 2], start = molas_dating_start[t], end = molas_dating_end[t], 
                         parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    wellcome_result <- rbind(wellcome_result, ind_result )
    
  }
  
  # saves results in Rda-object
  save(wellcome_result, file = file.path(".", saveFileDir, "Wellcome_result.Rda") )
}
load(file.path(".", saveFileDir, "Wellcome_result.Rda") )

wellcome_result <- rbind(wellcome_result, stbrides_crypt_full)

# range of Gompertz beta values
beta_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIlow, 4), "-",
                     round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIhigh, 4) )
# range of age modes M
M_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIlow, 1), "-",
                  round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIhigh, 1) )

wellcome_overview <- data.frame(cemetery = c(molas_cemeteries, "St. Bride's crypt (known age)", "St. Bride's crypt (osteological estimates)"),
                                beta = round(wellcome_result[which(wellcome_result$parameter == "beta"),]$Mode, 4),
                                beta_range, 
                                M = round(wellcome_result[which(wellcome_result$parameter == "M"),]$Mode, 1),
                                M_range)
wellcome_subset <- data.frame(cemetery = c(molas_cemeteries, "St. Bride's crypt (estimates)"),
                              wellcome_result[which(wellcome_result$parameter == "M" & wellcome_result$cemetery != "St. Bride's crypt (known age)"),])
wellcome_plot <- ggplot(wellcome_subset, aes(x = (start + end) / 2, y = Mode)) + 
  geom_errorbar(aes(ymin = HDIlow, ymax=  HDIhigh), width=15, colour = "dark grey") +
  geom_errorbarh(aes(xmax = start, xmin = end, height = 0), colour = "dark grey") +
  geom_point(aes(x = (start + end) / 2, y = Mode, group = cemetery, colour = cemetery )) + 
  xlab("year") + ylab("modal age") + ylim(10, 70)
