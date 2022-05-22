#medieval data, Russell, from https://genus.springeropen.com/articles/10.1186/s41118-021-00122-w/tables/2
peers <- read.table(file.choose(), header=TRUE, sep = "\t")

peers_subset <- subset(peers, age_begin >= 15)
peers_subset$death <- 1
surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age_begin - 15, death) ~ 1, data = peers_subset, dist="gompertz", weights = Deaths)
surv_lt_Gompertz_shape <- surv_lt$coefficients[1]
surv_lt_Gompertz_rate <- exp(surv_lt$coefficients[2])
