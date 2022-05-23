#######################################################
# Prerequisites
# Install required packages
require(pacman) || install.packages("pacman")
pacman::p_load(dplyr, fitdistrplus, flexsurv, ggplot2, gridExtra,
               mortAAR, nlme, reshape2,HMDHFDplus,Metrics,
               svMisc, tibble, tidyr, cowplot, MortalityLaws,
               coda, rjags, runjags)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

source("./helper_functions.R")

# Figure 1: Gompertz.
source("./gompertz_distribution.R")

# Figure 2: Map of Global History of Health samples
source("./BoE_map.R")
ggplot() +
  geom_sf(data = rivers_lakes_50m, colour = "light blue", size = 0.3) +
  geom_sf(data = lakes_50m, colour = "light blue", fill = "light blue", size = 0.2) +
  geom_sf(data = coastline_50m, colour = "dark grey", size = 0.2) +
  geom_sf(data = BoE_result_sf) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  coord_sf(ylim =  c(-1 + min_latitude, max_latitude + 1), xlim = c(-2 + min_longitude, max_longitude + 1), expand = FALSE)


# simulations_run.R
source("./lt_MC.R")
source("./gomp_bayes_known_age.R")
source("./gomp_anthr_age.R")

set.seed(209)
lt_sim <- lt.MC(sampling = 50,
                n_min = 50,
                n_max = 500,
                #M_min = 25,
                #M_max = 80,
                b_min = 0.025,
                b_max = 0.1,
                error_range = 15,
                age_categories = "BoE" 
)

lt_bias <- lt.sampling(1000,
                        n_min = 50,
                        n_max = 500,
                       b_min = 0.025,
                       b_max = 0.1,
                        error_range = 15) 

gridExtra::grid.arrange (
ggplot(lt_bias) + geom_point(aes(x = y, y = lt_bias_sd)),
ggplot(lt_bias) + geom_point(aes(x = y, y = lt_inaccuracy)),
ncol = 1
)
mean(lt_bias$lt_inaccuracy)

# Figure for Human Mortality Database, this requires credentials
source("./Human_Mortality_Database.R")
ggplot(lt_result, aes(x = year, y = beta)) + geom_point(aes(colour = country)) + ylab("Gompertz \u03B2") +
  geom_smooth(method='loess', span = 0.25, formula = y ~ x, colour = "red", se = TRUE, level = 0.95)

# Figure for historic life tables
source("./geneva.R")
source("./English_Mortality.R")
source("./halley_Breslau.R")
source("./suessmilch.R")
source("./Medieval_England.R")
source("./Germany.R")
source("./blayo_france.R")

comp_df <- rbind(London_result, Paris_result, halley_result, 
                 suessmilch_result, medieval_result, uelzen_result, blayo_result )
rownames(comp_df) <- NULL
cols.num <- c("year", "beta", "alpha")
comp_df[cols.num] <- sapply(comp_df[cols.num],as.numeric)

gridExtra::grid.arrange (
ggplot(geneva_result, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") + ylim(0.02, 0.06) +
  geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
  ggtitle("Geneva") +  theme(plot.title = element_text(hjust = 0.5)),
ggplot(eng_mort_result, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") + ylim(0.02, 0.06) +
  geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
  ggtitle("England") +  theme(plot.title = element_text(hjust = 0.5)),
ggplot(comp_df, aes(x = year, y = beta, group = group, colour = group, label = names ) ) + geom_point() + 
  ylab("Gompertz \u03B2") + xlab("years AD") + ggrepel::geom_text_repel(data = comp_df[comp_df$group != "France",]) + ylim(0.02, 0.06) + xlim(1400,1800) +
  ggtitle("Miscellaneous") +  theme(plot.title = element_text(hjust = 0.5)),
ncol= 2
)

# Figure 7: Global History of Health
source("./BoE_computation.R")
ggplot(subset(BoE_result, bayes_anthr_gomp_b > 0.01), aes(x = mean_century, y = bayes_anthr_gomp_b) ) + geom_point(aes(group = region, colour = region)) +
  geom_smooth(method='loess', span = 0.5, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) + 
  ylab("Gompertz \u03B2") + xlab("years AD") + xlim(200,1900) + ylim(0.01, 0.06)
