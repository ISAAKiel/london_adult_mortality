#alternative Daten von der Wellcome-Datenbank
molas_cemeteries <- c("Merton Priory", "St. Mary Graces", "St. Marylebone", "St. Bride's lower churchyard")
molas_dating_start <- c(1117, 1350, 1770, 1742)
molas_dating_end <- c(1538, 1540, 1849, 1817)

if (runCodeNew){
  set.seed(982)
  
  molas_data <- c("../Wellcome_Database/UPDATED_MERTON PRIORY_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021.xlsx", 
                  "../Wellcome_Database/UPDATED_ST MARY GRACES_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021.xlsx", 
                  "../Wellcome_Database/UPDATED_ST MARYLEBONE_MBH04_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021_joined.xlsx",
                  "../Wellcome_Database/UPDATED_ST BRIDES LOWER CHURCHYARD_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021.xlsx")
   
  wellcome_result <- data.frame()
  for (t in 1:length(molas_cemeteries) ) {
    molas_ind <- xls.amtl(molas_data[t])
    molas_ind <- as.data.frame(as.matrix(molas_ind))
    length_molas <- nrow(molas_ind)
    molas_ind$age_beg <-  NA
    molas_ind$age_end <-  NA
    for (i in 1:length_molas) {
      if(molas_ind$age[i] == 6) {
        molas_ind$age_beg[i] <-  12
        molas_ind$age_end[i] <-  18
      } else if(molas_ind$age[i] == 7) {
        molas_ind$age_beg[i] <-  18
        molas_ind$age_end[i] <-  26
      } else if(molas_ind$age[i] == 8) {
        molas_ind$age_beg[i] <-  26
        molas_ind$age_end[i] <-  36
      } else if(molas_ind$age[i] == 9) {
        molas_ind$age_beg[i] <-  36
        molas_ind$age_end[i] <-  46
      } else if(molas_ind$age[i] == 10) {
        molas_ind$age_beg[i] <-  46
        molas_ind$age_end[i] <-  100
      } else if(molas_ind$age[i] == 11) {
        molas_ind$age_beg[i] <-  18
        molas_ind$age_end[i] <-  100
      } 
    }
    
    #Bayesian modell with anthropological age estimate
    gomp.anthr_age(molas_ind, age_beg = "age_beg", age_end = "age_end",
                   thinSteps = 1, minimum_age = 12,
                   numSavedSteps = 400000) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind( cemetery = molas_cemeteries[t], start = molas_dating_start[t], end = molas_dating_end[t], 
                         parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    wellcome_result <- rbind(wellcome_result, ind_result )
    
  }

  # saves results in Rda-object
  save(wellcome_result, file = file.path(".", saveFileDir, "Wellcome_result.Rda") )
}
load(file.path(".", saveFileDir, "Wellcome_result.Rda") )

wellcome_result <- rbind(wellcome_result, stbrides_crypt_full, stbenet_result)

# range of Gompertz beta values
beta_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIlow, 4), "-",
                     round(wellcome_result[which(wellcome_result$parameter == "beta"),]$HDIhigh, 4) )
# range of age modes M
M_range <- paste0(round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIlow, 1), "-",
                  round(wellcome_result[which(wellcome_result$parameter == "M"),]$HDIhigh, 1) )

wellcome_overview <- data.frame(cemetery = c(molas_cemeteries, "St. Bride's crypt (known age)", "St. Bride's crypt (osteological estimates)", "St. Benet Sherehog"),
                                beta = round(wellcome_result[which(wellcome_result$parameter == "beta"),]$Mode, 4),
                                beta_range, 
                                M = round(wellcome_result[which(wellcome_result$parameter == "M"),]$Mode, 1),
                                M_range)

wellcome_plot <- ggplot(wellcome_result[which(wellcome_result$parameter == "M" & wellcome_result$cemetery != "St. Bride's crypt (known age)"),], 
       aes(x = (start + end) / 2, y = Mode)) + 
   geom_errorbar(aes(ymin = HDIlow, ymax=  HDIhigh), width=15, colour = "dark grey") +
  geom_errorbarh(aes(xmax = start, xmin = end, height = 0), colour = "dark grey") +
  geom_point(aes(x = (start + end) / 2, y = Mode, group = cemetery, colour = cemetery )) + 
               xlab("year") + ylab("modal age") + ylim(10, 70)
