source("./lifetables_processing/English_Mortality.R")
source("./lifetables_processing/Medieval_England.R")
source("./lifetables_processing/London_1841_raw_all.R")
source("./lifetables_processing/HMD_UK.R")
source("./lifetables_processing/London_1758.R")
source("./lifetables_processing/Landers_1997_London.R")
source("./lifetables_processing/English_Peers.R")

# modal age M
england <- data.frame(source = "Family Reconstitution", eng_mort_result[which(eng_mort_result$parameter == "M"),c(1,7)])
London_1841 <- data.frame(source = "London 1841", year = "X1841", Mode = London_1841_result[3,5])
HMD_UK <- data.frame(source = "HMD UK", HMD_UK_result[which(HMD_UK_result$parameter == "M"),c(1,7)])
english_mortality <- rbind(england, London_1841, HMD_UK)
english_mortality_prep <- data.frame(source = "written", data = english_mortality$source, 
                                     M = english_mortality$Mode, year = english_mortality$year,
                                     start = NA, end = NA, HDIlow = NA, HDIhigh = NA)
english_mortality_prep_r <- rbind(Peers_prep, monks_prep, London_1758_prep_r, english_mortality_prep)
english_mortality_prep <- rbind(Peers_prep, monks_prep, London_1758_prep, english_mortality_prep)
english_mortality_M <- ggplot(english_mortality, aes(x = as.numeric(substr(year, 2, 5)), y = Mode, colour = source ) ) + geom_point() + 
  ylab("modal age") + xlab("year") + ylim(10, 70)

# Gompertz beta
england_beta <- data.frame(source = "Family Reconstitution", eng_mort_result[which(eng_mort_result$parameter == "beta"),c(1,7)])
London_1841_beta <- data.frame(source = "London 1841", year = "X1841", Mode = London_1841_result[2,5])
HMD_UK_beta <- data.frame(source = "HMD UK", HMD_UK_result[which(HMD_UK_result$parameter == "beta"),c(1,7)])
english_mortality_beta <- rbind(england_beta, London_1841_beta, HMD_UK_beta)
english_mortality_beta_prep <- data.frame(source = "written", data = english_mortality_beta$source, 
                                     beta = english_mortality_beta$Mode, year = english_mortality_beta$year,
                                     start = NA, end = NA, HDIlow = NA, HDIhigh = NA)
english_mortality_beta_prep <- rbind(Peers_beta_prep, monks_beta_prep, London_1758_beta_prep, english_mortality_beta_prep)
