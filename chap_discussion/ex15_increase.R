# London population increase between 1650 and 1841 (Greater London!)
london_increase <- (2207653 - 375000) / 375000

# in the 1841 census, 1,293,969 Inner Londoners (not Greater London) were of age 15 or above, 
# while 576,449 were below this threshold
age15_perc <- 1293969 / (576449 + 1293969)

# Life expectancy for New Churchyard and London 1841 at the entrance to adulthood (15/12 years)
# note that life expectancy for New Churchyard was calculated for age 12,
# so we have to calculate alpha for age 15

new_churchyard_a15 <- 0.021205430 * exp(0.0401 * 3)
ex_london1841 <- gomp.ex(15, 0.004512645, 0.054650697, age_start = 15)
ex_new_churchyard <- gomp.ex(15, new_churchyard_a15, 0.0401, age_start = 15)
ex_rel <- (ex_london1841 - ex_new_churchyard) / ex_new_churchyard

# share of later adult mortality in population increase
ex_increase <- ex_rel * age15_perc
mort_explain <- ex_increase / london_increase 
mort_explain_share <- 1 / mort_explain
