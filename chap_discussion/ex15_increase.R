# London population increase between 1650 and 1841 (Greater London!)
london_increase <- (2207653 - 375000) / 375000

# in the 1841 census, 1,293,969 Inner Londoners (not Greater London) were of age 15 or above, 
# while 576,449 were below this threshold
age15_perc <- 1293969 / (576449 + 1293969)

# Life expectancy for New Churchyard and London 1841 at the entrance to adulthood (15/12 years)
# note that life expectancy for New Churchyard was calculated for age 12,
# so we have to calculate alpha for age 15
new_churchyard_a12 <- 0.021205430
new_churchyard_b <- 0.0401

london_1841a <- 0.004512645
london1841_b <- 0.054650697

new_churchyard_a15 <- 0.021205430 * exp(new_churchyard_b * 3)
ex_london1841 <- gomp.ex(15, london_1841a, london1841_b, age_start = 15)
ex_new_churchyard <- gomp.ex(15, new_churchyard_a15, new_churchyard_b, age_start = 15)
ex_rel <- (ex_london1841 - ex_new_churchyard) / ex_new_churchyard

# share of later adult mortality in population increase
ex_increase <- ex_rel * age15_perc
mort_explain <- ex_increase / london_increase 
mort_explain_share <- 1 / mort_explain


# share of fertility
# New Churchyard data
new_churchyard_fert <- fertility_survival_mixture(age_a = new_churchyard_a15, age_b = new_churchyard_b, lower = 0, upper = 35)

# London 1841 data
london1841_fert <- fertility_survival_mixture(age_a = london_1841a, age_b = london1841_b, lower = 0, upper = 35)

fert_rel <- (london1841_fert - new_churchyard_fert) / new_churchyard_fert
fert_explain <- fert_rel / london_increase 
fert_explain_share <- 1 / fert_explain
