source("./lifetables_processing/English_Mortality.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
source("./lifetables_processing/Medieval_England.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
source("./lifetables_processing/London_1841_raw_all.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
source("./lifetables_processing/HMD_UK.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
source("./lifetables_processing/English_Peers.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
source("./lifetables_processing/London_1728_1840.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))

# modal age M
england <- data.frame(source = "Family Reconstitution", 
                      eng_mort_result[which(eng_mort_result$parameter == "M"),
                                      c(1,7)])
London_1841 <- data.frame(source = "London 1841", 
                          year = "X1841", 
                          Mode = London_1841_result[3,5])
HMD_UK <- data.frame(source = "HMD UK", 
                     HMD_UK_result[which(HMD_UK_result$parameter == "M"),
                                   c(1,7)])
London_1728_1840 <- data.frame(source = "London 1728-1840", 
                               london_1728_1840_result[which(london_1728_1840_result$parameter == "M"),
                                                       c(1,7)])
London_1728_1840_r <- data.frame(source = "London 1728-1840", 
                                 london_1728_1840_result_r[which(london_1728_1840_result_r$parameter == "M"),
                                                           c(1,7)])
english_mortality <- rbind(england, 
                           London_1728_1840, 
                           London_1841, 
                           HMD_UK)
english_mortality_prep <- data.frame(source = "written", 
                                     data = english_mortality$source, 
                                     M = english_mortality$Mode, 
                                     year = english_mortality$year,
                                     start = NA, end = NA, HDIlow = NA, HDIhigh = NA)
english_mortality_prep <- rbind(peers_prep, 
                                monks_prep, 
                                english_mortality_prep)
english_mortality_r <- rbind(england, 
                             London_1728_1840_r, 
                             London_1841, 
                             HMD_UK)
english_mortality_prep_r <- data.frame(source = "written", 
                                       data = english_mortality_r$source, 
                                       M = english_mortality_r$Mode, 
                                       year = english_mortality_r$year,
                                       start = NA, end = NA, HDIlow = NA, HDIhigh = NA)
english_mortality_prep_r <- rbind(peers_prep, 
                                  monks_prep, 
                                  english_mortality_prep_r)

# Gompertz beta
england_beta <- data.frame(source = "Family Reconstitution", 
                           eng_mort_result[which(eng_mort_result$parameter == "beta"),c(1,7)])
London_1841_beta <- data.frame(source = "London 1841", 
                               year = "X1841", 
                               Mode = London_1841_result[2,5])
HMD_UK_beta <- data.frame(source = "HMD UK", 
                          HMD_UK_result[which(HMD_UK_result$parameter == "beta"),
                                        c(1,7)])
english_mortality_beta <- rbind(england_beta, 
                                London_1841_beta, 
                                HMD_UK_beta)
english_mortality_beta_prep <- data.frame(source = "written", 
                                          data = english_mortality_beta$source,
                                          beta = english_mortality_beta$Mode, 
                                          year = english_mortality_beta$year,
                                          start = NA, end = NA, HDIlow = NA, HDIhigh = NA)
english_mortality_beta_prep <- rbind(peers_beta_prep, 
                                     monks_beta_prep,
                                     english_mortality_beta_prep)
