source("./lifetables_processing/English_Mortality.R")
source("./lifetables_processing/Medieval_England.R")
source("./lifetables_processing/London_1841_raw_all.R")
source("./lifetables_processing/Landers_1997_London_series.R")
source("./lifetables_processing/HMD_UK.R")

# Gompertz beta
monks_df <- data.frame(group = "Monks", year = "X1450", Mode = monks_result[2,5])
england <- data.frame(group = "England", eng_mort_result[which(eng_mort_result$parameter == "beta"),c(1,7)])
London <- data.frame(group = "London", London_Landers_result[which(London_Landers_result$parameter == "beta"),c(1,7)])
London_1841 <- data.frame(group = "London 1841", year = "X1841", Mode = London_1841_result[2,5])
english_mortality <- rbind(monks_df, england, London, London_1841)

ggplot(english_mortality, aes(x = as.numeric(substr(year, 2, 5)), y = Mode, colour = source ) ) + geom_point() + 
  ylab("Gompertz \u03B2") + xlab("year") + ylim(0.025, 0.075)

# modal age M
monks_df <- data.frame(source = "Monks", year = "X1450", Mode = monks_result[3,5])
england <- data.frame(source = "Family Reconst.", eng_mort_result[which(eng_mort_result$parameter == "M"),c(1,7)])
London <- data.frame(source = "London (Landers)", London_Landers_result[which(London_Landers_result$parameter == "M"),c(1,7)])
London_1841 <- data.frame(source = "London 1841", year = "X1841", Mode = London_1841_result[3,5])
HMD_UK <- data.frame(source = "HMD UK", HMD_UK_result[which(HMD_UK_result$parameter == "M"),c(1,7)])
english_mortality <- rbind(monks_df, england, London, London_1841, HMD_UK)

english_mortality_M <- ggplot(english_mortality, aes(x = as.numeric(substr(year, 2, 5)), y = Mode, colour = source ) ) + geom_point() + 
  ylab("modal age") + xlab("year") + ylim(10, 70)
