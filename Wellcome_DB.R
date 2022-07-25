#alternative Daten von der Wellcome-Datenbank
if (runCodeNew){
  set.seed(982)
  
  molas_data <- c("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Wellcome_Database/UPDATED_EAST SMITHFIELD BLACK DEATH_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021.xlsx", "/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Wellcome_Database/UPDATED_MERTON PRIORY_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021.xlsx", "/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Wellcome_Database/UPDATED_ST MARY GRACES_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021.xlsx", "/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Wellcome_Database/UPDATED_ST BRIDES LOWER CHURCHYARD_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021.xlsx", "/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Wellcome_Database/UPDATED_ST MARYLEBONE_MBH04_DENTAL DATA_NILS MULLER SCHEESSEL_MAY 2021_joined.xlsx")
  molas_cemeteries <- c("East Smithfield Black Death", "Merton Priory (Medieval)", "St. Mary (Medieval)", "St. Brides (18th-19th c.)", "St. Marylebone (19th c.)")
  
  lt_result <- data.frame()
  for (t in 1:length(molas_cemeteries) ) {
    molas_ind <- xls.amtl(molas_data[t])
    molas_ind <- as.data.frame(as.matrix(molas_ind))
    length_molas <- nrow(molas_ind)
    molas_ind$age_beg <-  NA
    molas_ind$age_end <-  NA
    for (i in 1:length_molas) {
      if(molas_ind$age[i] == 7) {
        molas_ind$age_beg[i] <-  18
        molas_ind$age_end[i] <-  25
      } else if(molas_ind$age[i] == 8) {
        molas_ind$age_beg[i] <-  26
        molas_ind$age_end[i] <-  35
      } else if(molas_ind$age[i] == 9) {
        molas_ind$age_beg[i] <-  36
        molas_ind$age_end[i] <-  45
      } else if(molas_ind$age[i] == 10) {
        molas_ind$age_beg[i] <-  46
        molas_ind$age_end[i] <-  99
      } else if(molas_ind$age[i] == 11) {
        molas_ind$age_beg[i] <-  18
        molas_ind$age_end[i] <-  99
      } 
    }
    
    #Bayesian modell with anthropological age estimate
    bayes_anthr_gomp_b <- NA
    bayes_anthr_gomp_a <- NA
    gomp.anthr_age(molas_ind, age_beg = "age_beg", age_end = "age_end",
                   thinSteps = 1, minimum_age = 18,
                   numSavedSteps = 10000) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    bayes_anthr_gomp_b <- gomp_anthr_MCMC_diag[2,5]
    bayes_anthr_gomp_a <- gomp_anthr_MCMC_diag[1,5]
    
    ind_result <- cbind(molas_cemeteries[t], bayes_anthr_gomp_b, bayes_anthr_gomp_a)
    lt_result <- rbind(lt_result, ind_result)
  }
  rownames(lt_result) <- NULL
  colnames(lt_result) <- c("cemetery", "Gompertz beta", "Gompertz alpha")
  cols.num <- c("Gompertz beta", "Gompertz alpha")
  lt_result[cols.num] <- sapply(lt_result[cols.num],as.numeric)
  Wellcome_result <- lt_result
  # saves results in Rda-object
  save(lt_result, file = file.path(".", saveFileDir, "Wellcome_result.Rda") )
}
load(file.path(".", saveFileDir, "Wellcome_result.Rda") )
