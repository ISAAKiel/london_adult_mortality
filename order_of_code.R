#######################################################
# Prerequisites
# Install required packages
require(pacman) || install.packages("pacman")
pacman::p_load(dplyr, fitdistrplus, flexsurv, ggplot2, gridExtra, kableExtra,
               mortAAR, nlme, reshape2, rgdal, HMDHFDplus, Metrics,
               svMisc, tibble, tidyr, cowplot, MortalityLaws, rio,
               coda, rjags, runjags, demogR, sf, rnaturalearth, readxl,
               ggrepel)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

source("./helper_functions.R")
source("./lt_MC.R")
source("./gomp_bayes_known_age.R")
source("./gomp_anthr_age.R")
source("./Gomp_MLE.R")
source("./Gomp_MLE_adapted.R")
source("./Gomp_MLE_interval.R")
source("./lt_MC_Gomp.R")
RNGkind("L'Ecuyer-CMRG") # conservative random number generator to avoid periodicity

# run extensive code anew. Set TRUE to run extensive code (6 h +)
runCodeNew <- FALSE
runCodeNew <- TRUE

# Specify filename prefix for saved files and create a folder if needed:
saveFileDir = "preprocessed_files"
dir.create(file.path(".", saveFileDir), showWarnings = FALSE )


#############
# Methods

# Figure 1: Gompertz.
source("./gompertz_distribution.R")


#############
# Data

# computing the inaccuracy of resampled "age estimations"
lt_bias <- lt.sampling(1000, n_min = 50, n_max = 500, b_min = 0.02, b_max = 0.1, error_range = 15) 
mean(lt_bias$lt_inaccuracy)


############
# Results

# Simulations
source("./simulations_run.R")
# plot of difference between expected and observed value
do.call(gridExtra::grid.arrange, plot_list_diff)
# table of RMSEs
rmse_result[order(rmse_result$RMSE) ,]

# plot of results of methods with estimated age-at-death
do.call(gridExtra::grid.arrange, plot_list_estim_shapes)
# table of RMSEs
rmse_estim_result[order(rmse_estim_result$RMSE) ,]
# plot for Bayesian model of difference
do.call(gridExtra::grid.arrange, c(plot_list_bayes_diff, ncol = 2) )


## Historical Data
# Written sources, pre-processed
source("./historical_lifetables.R")
London_series_ranges
London_1841_ranges
eng_mort_ranges
HMD_UK_ranges
monks_ranges

# Wellcome Data
source("./lifetables_processing/stbrides_crypt.R")
source("./Wellcome_DB.R") # can take a while
# St. Bride's crypt data, comparison of known age and osteological estimates
gridExtra::grid.arrange(stbrides_crypt_plot,
                        bottom = "black = density of actual ages (bandwidth = 5)\n blue = Gompertz distribution of actual ages\n red = Gompertz distribution of osteological estimates")

# show overview of Wellcome data
wellcome_overview

# modal ages from historical and osteological data
gridExtra::grid.arrange(english_mortality_M, wellcome_plot, ncol = 2)


############
# Discussion

# Minimum Gompertz beta in Coale/Demeny-Tables
source("./coale_demeny_life_tables_gompertz.R")
min(gompertz_df$Gompertz_shape)


##############
# Supplement

# one complete Bayesian example, with different settings
set.seed(1312)
source("./bayes_complete.R") # can take a few minutes
bayes_complete_table

source("./simulations_run.R")
# plot of results of methods with known age-at-death
do.call(gridExtra::grid.arrange, plot_list_shapes)

# simulation of population increase
source("./simulations_pop_incr_run.R")
do.call(gridExtra::grid.arrange, c(lt_sim_list, ncol = 6) )

# Written sources, pre-processed
source("./historical_lifetables.R")
London_Landers_result
London_1841_result
eng_mort_result
HMD_UK_result
monks_result

# Mortality in the Wellcome dataset, pre-processed
source("./lifetables_processing/stbrides_crypt.R")
source("./Wellcome_DB.R")
wellcome_result
