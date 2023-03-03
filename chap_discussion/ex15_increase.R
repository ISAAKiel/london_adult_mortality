# London population increase between 1650 and 1841 (Greater London!)
london_increase <- (2207653 - 375000) / 375000

# In the 1841 census, 1,293,969 Inner Londoners (not Greater London) were of age 15 or above, 
# while 576,449 were below this threshold
age15_perc <- 1293969 / (576449 + 1293969)

# Life expectancy for New Churchyard and London 1841 at the entrance to adulthood (15/12 years)
# note that life expectancy for New Churchyard was calculated for age 12,
# so we have to calculate alpha for age 15
new_churchyard_a12 <- 0.021205430
new_churchyard_b <- 0.0401

london_1841_a <- 0.004512645
london_1841_b <- 0.054650697

new_churchyard_a15 <- 0.021205430 * exp(new_churchyard_b * 3)
# gomp.ex() s. helper_functions.R
ex_london_1841 <- gomp.ex(15, london_1841_a, london_1841_b, age_start = 15)
ex_new_churchyard <- gomp.ex(15, new_churchyard_a15, new_churchyard_b, age_start = 15)
ex_rel <- (ex_london_1841 - ex_new_churchyard) / ex_new_churchyard

# Share of later adult mortality in population increase
ex_increase <- ex_rel * age15_perc
mort_explain <- ex_increase / london_increase 
mort_explain_share <- 1 / mort_explain


# Share of fertility
# New Churchyard data
# fertility_survival_mixture() s. helper_functions.R
new_churchyard_fert <- fertility_survival_mixture(age_a = new_churchyard_a15, 
                                                  age_b = new_churchyard_b, 
                                                  lower = 0, upper = 35)

# London 1841 data
london1841_fert <- fertility_survival_mixture(age_a = london_1841_a, 
                                              age_b = london_1841_b, 
                                              lower = 0, upper = 35)

fert_rel <- (london1841_fert - new_churchyard_fert) / new_churchyard_fert

# the following calculations are nice but actually unnecessary
# "natural" child spacing according to Hassan 1981, 127: 27.3 months, 32 years reproductive period
# child_spac <- 27.3 / 12
# tfr <- 32 / child_spac
# new_churchyard_trf <- new_churchyard_fert * tfr
# london1841_trf <- london1841_fert * tfr
# 
# # percentage female children, Hassan 1981, 137: 0.488, then gross reproduction rate
# new_churchyard_grr <- new_churchyard_trf * 0.488
# london1841_grr <- london1841_trf * 0.488
# 
# # net reproduction rate, with 50 per cent of individuals dying before 15 (Hassan 1981, 139):
# new_churchyard_nrr <- new_churchyard_grr * 0.5
# london1841_nrr <- london1841_grr * 0.5

# increase in net reproduction rate per year in 191 years
nrr_increase <- fert_rel/ 191

nrr_increase_exp <- (1 + nrr_increase)^191
fert_explain <- (nrr_increase_exp - 1) / london_increase 
fert_explain_share <- 1 / fert_explain

explain_sum <- fert_explain + mort_explain
