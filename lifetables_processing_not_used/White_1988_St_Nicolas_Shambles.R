# St. Nicholas Shambles, form White 1988, Appendix 1
if (runCodeNew){
  nick_mort <- read.table("./data/White_St_Nicholas_Shambles_1988.txt", header=TRUE, sep = "\t")
  nick_mort <- nick_mort[,c(1,4)]
  unique(nick_mort$age)
  nick_mort$age_clean <- ifelse(nick_mort$age == "adult", "20-100", 
                                ifelse(nick_mort$age == "45+", "45-100",
                                       ifelse(nick_mort$age == "infant", "0-12",
                                              ifelse(nick_mort$age == "foetus", "0-1",
                                                     ifelse(nick_mort$age == "9", "8-10",
                                                            ifelse(nick_mort$age == "40", "35-45",
                                                                   ifelse(nick_mort$age == "45", "40-50",
                                                                          ifelse(nick_mort$age == "<4.5", "0-4",
                                                                                 ifelse(nick_mort$age == "25", "20-30",
                                                                                        ifelse(nick_mort$age == "1", "0-1",
                                                                                               ifelse(nick_mort$age == "8", "7-9",
                                                                                                      ifelse(nick_mort$age == "2", "1-3",
                                                                                                             ifelse(nick_mort$age == "juvenile", "12-18",
                                                                                                                    ifelse(nick_mort$age == "7", "6-8",
                                                                                                                           ifelse(nick_mort$age == "adolescent","12-18",
                                                                                                                                  ifelse(nick_mort$age == "<25","18-25",
                                                                                                                                         ifelse(nick_mort$age == "30","25-35",
                                                                                                                                                ifelse(nick_mort$age == "6", "5-7",
                                                                                                                                                       ifelse(nick_mort$age == "<40", "18-40",
                                                                                                                                                              ifelse(nick_mort$age == "1.5" | nick_mort$age == "1-1.5","1-2",
                                                                                                                                                                     ifelse(nick_mort$age == "4", "3-5",
                                                                                                                                                                            ifelse(nick_mort$age == "17","16-18",
                                                                                                                                                                                   ifelse(nick_mort$age == "0.5","0-1",
                                                                                                                                                                                          ifelse(nick_mort$age == "38+", "38-100",
                                                                                                                                                                                                 ifelse(nick_mort$age == "neonate", "0-1",nick_mort$age)))))))))))))))))))))))))
  nick_mort <- nick_mort %>% tidyr::separate(age_clean, c("agebeg", "ageend"), sep = "-") %>%
    transform(agebeg = as.numeric(agebeg), ageend = as.numeric(ageend) ) 
  nick_mort_sub <- subset(nick_mort, agebeg >=12)
  
  gomp.anthr_age(nick_mort_sub, age_beg = "agebeg", age_end = "ageend",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 100000,
                 minimum_age = 12) %>%
    diagnostic.summary(., HDImass = 0.95) -> St_Nicolas_diag
  # saves results in Rda-object
  save(St_Nicolas_diag, file = file.path(".", saveFileDir, "St_Nicolas_diag.Rda") )
}
load(file.path(".", saveFileDir, "St_Nicolas_diag.Rda") )

bayes_anthr_gomp_b <- St_Nicolas_diag[2,5]
bayes_anthr_gomp_a <- St_Nicolas_diag[1,5]

St_Nicolas <- cbind(beta = bayes_anthr_gomp_b, alpha = bayes_anthr_gomp_a)
