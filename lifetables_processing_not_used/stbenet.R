## https://www.museumoflondon.org.uk/collections/other-collection-databases-and-libraries/centre-human-bioarchaeology/osteological-database/post-medieval-cemeteries/st-benet-sherehog-post-medieval
# St. Benet Sherehog cemetery 
if (runCodeNew){
  set.seed(522)
  age_beg <- c(12, 18, 26, 36, 46, 18)
  age_end <- c(18, 26, 36, 46, 100, 100)
  dx <- c(12, 9, 33, 50, 32, 43)
  stbenet <- data.frame(age_beg, age_end, dx)
  
  year_data_uncount <- stbenet %>% uncount(dx)
  
  gomp.anthr_age(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 200000,
                 minimum_age = 12) %>%
    diagnostic.summary(., HDImass = 0.95) -> stbenet_result
  
  # saves results in Rda-object
  save(stbenet_result, file = file.path(".", saveFileDir, "stbenet_result.Rda") )
}
load(file.path(".", saveFileDir, "stbenet_result.Rda") )

stbenet_result <- cbind(cemetery = "St. Benet Sherehog", 
                                   start = 1500, end = 1699,
                                   parameter = c("alpha", "beta", "M"), stbenet_result)
rownames(stbenet_result) <- NULL
