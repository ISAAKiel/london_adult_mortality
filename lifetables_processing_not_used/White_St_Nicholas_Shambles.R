white <- read.table("./data/White_St_Nicholas_Shambles_1988_30_Fig_30.txt", header=TRUE, sep = "\t")
shambles <- white[,5]
age_beg = c( 12, 18, 26, 36, 46, 18)
age_end = c(18, 26, 36, 46, 120, 120)
shambles_df <- data.frame(age_beg, age_end, shambles)

year_data_uncount <- shambles_df %>% uncount(shambles)
cem_dates <- c(1000, 1200)
london_sub <- subset(london_df, year >= cem_dates[1] & year < cem_dates[2])
pop_rate <- psych::geometric.mean(london_sub$rate) - 1

gomp.anthr_age.r(year_data_uncount, age_beg = "age_beg", age_end = "age_end",
               silent.jags = FALSE,
               silent.runjags = FALSE,
               thinSteps = 1,
               numSavedSteps = 100000,
               minimum_age = 12, r = pop_rate) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag

ggplot(shambles_df[-6,]) + xlim(12,100) + #geom_line(aes(x = (age_beg + age_end) / 2, y = shambles)) +
geom_function(fun = function(x) flexsurv::dgompertz(x - 12, 0.02864833, 0.02606075), colour = "red")
