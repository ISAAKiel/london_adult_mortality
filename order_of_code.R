#######################################################
# Prerequisites
# Install required packages
require(pacman) || install.packages("pacman")
pacman::p_load(dplyr, fitdistrplus, flexsurv, ggplot2, gridExtra, kableExtra,
               mortAAR, nlme, reshape2, rgdal, HMDHFDplus, Metrics,
               svMisc, tibble, tidyr, cowplot, MortalityLaws, rio,
               coda, rjags, runjags, demogR, sf, rnaturalearth)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

source("./helper_functions.R")
source("./lt_MC.R")
source("./gomp_bayes_known_age.R")
source("./gomp_anthr_age.R")
source("./Gomp_MLE_interval.R")
RNGkind("L'Ecuyer-CMRG") # conservative random number generator to avoid periodicity

# run extensive code anew. Set TRUE to run extensive code (6 h +)
runCodeNew <- FALSE
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
lt_bias <- lt.sampling(1000, n_min = 50, n_max = 500, b_min = 0.025, b_max = 0.1, error_range = 15) 
mean(lt_bias$lt_inaccuracy)

# Global History of Health samples
source("./BoE_data.R")
# table of individuals and samples of Global History of Health
BoE_table_ind
BoE_table_sites
source("./BoE_map.R") # download of data from naturalearthdata
BoE_map


############
# Results

# Simulations
source("./simulations_run.R")
# plot of results of methods with known age-at-death
do.call(gridExtra::grid.arrange, plot_list_shapes)
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


# Human Mortality Database, this requires credentials
source("./Human_Mortality_Database.R")
HMD_plot


# Historic life tables
source("./historical_lifetables.R")
do.call(gridExtra::grid.arrange, c(hist_lt, ncol = 3))


# Global History of Health
source("./BoE_computation.R") # can take a while
plot_all # all sites in one

# plot it for the regions without Mediterranean (only 4 sites)
do.call(gridExtra::grid.arrange, c(plot_list, ncol = 3))

# show table
BoE_result[order((BoE_result$period) ), ] %>% 
  as.data.frame.matrix() %>%
  knitr::kable(., caption = "Global History of health")  %>%
  kableExtra::column_spec(., 1:13, width= "3cm")


############
# Discussion

# plot bad age diagrams of BoE
do.call(gridExtra::grid.arrange, c(plot_list_bad, ncol = 3))
# plot 9 randomly selected good age diagrams
do.call(gridExtra::grid.arrange, c(plot_list_good, ncol = 3))

# Minimum Gompertz beta in Coale/Demeny-Tables
source("./coale_demeny_life_tables_gompertz.R")
min(gompertz_df$Gompertz_shape)


##############
# Supplement

# one complete Bayesian example, with different settings
set.seed(1312)
source("./bayes_complete.R") # can take a few minutes
bayes_complete_table