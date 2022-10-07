molas_cemeteries <- c("Merton Priory", #"Merton Priory", "Merton Priory", "Merton Priory", "Merton Priory",
                      "Bermondsey Abbey", "East Smithfield", "St. Mary Graces", 
                      #"St. Mary Graces OA2", "St. Mary Graces OA9", "St. Mary Graces church", # samples get too small
                      "Broadgate", #"Broadgate Phase 1", "Broadgate Phase 2",
                      "St. Benet Sherehog", "Chelsea Old church", "St. Marylebone", "St. Bride's lower churchyard",
                      "Sheen's burial ground", "Bow Baptist Church", 
                      # "Bunhill", # age distribution is irregular
                      "St. Mary and St. Michael")

if (runCodeNew){
  set.seed(982)
  age_beg <- c(12, 18, 26, 36, 46, 18)
  age_end <- c(18, 26, 36, 46, 100, 100)
  #guildhall <- c(4, 7, 10, 11, 6, 13) # too small
  bermondsey_abbey <- c(1, 15, 28, 51, 37, 69)
  east_smithfield <- c(53, 63, 121, 97, 22, 117)
  st_mary <- c(24, 30, 58, 69, 40, 77)
  # st_mary_OA2 <- c(15, 19, 32, 37, 12, 27)
  # st_mary_OA9 <- c(4, 8, 12, 13, 10, 21)
  # st_mary_church <- c(5, 3, 14, 19, 18, 29)
  # https://archaeologydataservice.ac.uk/catalogue/adsdata/arch-3331-1/dissemination/reports/XSM10_osteo_tab01.pdf
  broadgate <- c(53, 126, 153, 132, 66, 26)
  # broadgate_phase1 <- c(34, 71, 75, 77, 31, 15)
  # broadgate_phase2 <- c(19, 55, 78, 55, 35, 11)
  st_benet <- c(12, 9, 33, 50, 32, 43)
  chelsea_old_church <- c(3, 14, 17, 46, 72, 16)
  st_marylebone <- c(3, 20, 42, 69, 52, 40)
  st_brides_lower <- c(10, 10, 44, 88, 162, 65)
  sheen_burial_ground <- c(6, 13, 26, 32, 41, 54)
  bow_baptist <- c(17, 25, 51, 64, 50, 24)
   # bunhill <- c(6, 20, 41, 14, 35, 7)
  st_mary_and_michael <- c(16, 33, 81, 92, 41, 21)
  molas_dating_start <- c(1089, 1348, 1350, 1569,
                          # 1350, 1400, 1361, 
                          1670, 1712, 1742, 1770, 1763, 1816, 
                          # 1833, 
                          1843)
  molas_dating_end <- c(1538, 1350, 1540, 1739,
                        # 1400, 1539, 1539, 
                        1740, 1842, 1817, 1849, 1854, 1854, 
                        #1853, 
                        1853)
  wellcome_data <- data.frame(age_beg, age_end, #guildhall, 
                              bermondsey_abbey, east_smithfield, st_mary, 
                              # st_mary_OA2,st_mary_OA9, st_mary_church, 
                              broadgate, st_benet, chelsea_old_church, 
                              st_marylebone, st_brides_lower, sheen_burial_ground,
                              bow_baptist, 
                              # bunhill,
                              st_mary_and_michael)
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

wellcome_result <- rbind(merton_result, wellcome_result, stbrides_crypt_full)

# range of Gompertz beta values
beta_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIlow, 4), "-",
                     round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIhigh, 4) )
# range of age modes M
M_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIlow, 1), "-",
                  round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIhigh, 1) )

# expectation of life
wellcome_names <- unique(wellcome_result$cemetery)
wellcome_20ex <- NULL
wellcome_25ex <- NULL
for (k in wellcome_names) {
  wellcome_sub <- subset(wellcome_result, cemetery == k)
  if(k < 6) {
    start_age <- 13
  } else {
    start_age <- 12
  }
  w20ex <- gomp.ex(20, wellcome_sub[1,9], wellcome_sub[2,9], age_start = start_age)
  w25ex <- gomp.ex(25, wellcome_sub[1,9], wellcome_sub[2,9])
  wellcome_20ex <- c(wellcome_20ex, w20ex)
  wellcome_25ex <- c(wellcome_25ex, w25ex)
}

wellcome_overview <- data.frame(cemetery = c(molas_cemeteries, "St. Bride's crypt (known age)", "St. Bride's crypt (osteological estimates)"),
                                beta = round(wellcome_result[which(wellcome_result$parameter == "beta"),]$Mode, 4),
                                beta_range, 
                                M = round(wellcome_result[which(wellcome_result$parameter == "M"),]$Mode, 1),
                                M_range, ex20 = round(wellcome_20ex, 1), ex25 = round(wellcome_25ex, 1))
wellcome_subset <- data.frame(cemetery = c(molas_cemeteries, "St. Bride's crypt (estimates)"),
                              wellcome_result[which(wellcome_result$parameter == "M" & wellcome_result$cemetery != "St. Bride's crypt (known age)"),])
wellcome_prep <- data.frame(source = "osteological", wellcome_subset[,c(1, 10)], year = NA,
                            wellcome_subset[,c(3, 4, 14, 15)])
colnames(wellcome_prep) <- c("source", "data", "M", "year", "start", "end", "HDIlow", "HDIhigh")

wellcome_plot <- ggplot(wellcome_subset, aes(x = (start + end) / 2, y = Mode)) + 
  geom_errorbar(aes(ymin = HDIlow, ymax=  HDIhigh), width=15, colour = "dark grey") +
  geom_errorbarh(aes(xmax = start, xmin = end, height = 0), colour = "dark grey") +
  geom_point(aes(x = (start + end) / 2, y = Mode, group = cemetery, colour = cemetery )) + 
  xlab("year") + ylab("modal age") + ylim(10, 70)

wellcome_subset_beta <- data.frame(cemetery = c(molas_cemeteries, "St. Bride's crypt (estimates)"),
                              wellcome_result[which(wellcome_result$parameter == "beta" & wellcome_result$cemetery != "St. Bride's crypt (known age)"),])
wellcome_prep_beta <- data.frame(source = "osteological", wellcome_subset_beta[,c(1, 10)], year = NA,
                                 wellcome_subset_beta[,c(3, 4, 14, 15)])
colnames(wellcome_prep_beta) <- c("source", "data", "beta", "year", "start", "end", "HDIlow", "HDIhigh")
