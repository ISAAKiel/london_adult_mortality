#######################################################
# Prerequisites
# Install required packages
require(pacman) || install.packages("pacman")
pacman::p_load(coda, cowplot, demogR, dplyr, fitdistrplus, flexsurv, ggplot2, 
               ggrepel, gridExtra, HMDHFDplus, kableExtra, Metrics, mortAAR, 
               MortalityLaws, nlme, osmplotr, psych, readxl, reshape2, rgdal, 
               rio, rjags, rnaturalearth, runjags, sf, svMisc, tibble, tidyr)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

source("./helper_functions.R")
source("./lt_MC.R")
source("./gomp_bayes_known_age.R")
source("./gomp_anthr_age.R")
source("./gomp_known_age_r.R")
source("./gomp_anthr_age_r.R")
source("./Gomp_MLE.R")
source("./Gomp_MLE_adapted.R")
source("./Gomp_MLE_interval.R")
source("./lt_MC_Gomp.R")
source("./bayes_cat_poisson.R")
RNGkind("L'Ecuyer-CMRG") # conservative random number generator to avoid periodicity

# run extensive code anew. Set TRUE to run extensive code (6 h +)
runCodeNew <- FALSE
#runCodeNew <- TRUE

# Specify filename prefix for saved files and create a folder if needed:
saveFileDir = "preprocessed_files"
if (saveFileDir %in% list.files(getwd())) {
  # Dir exists
}else{
  dir.create(file.path(".", saveFileDir), showWarnings = FALSE )
}

#############
# Methods

# Figure 1: Gompertz.
source("./chap_methods/gompertz_distribution.R")

# Minimum Gompertz beta in Coale/Demeny-Tables
source("./chap_methods/coale_demeny_life_tables_gompertz.R")
min(gompertz_df$Gompertz_shape)

# hazard curve (mx) to show turning point
source("./chap_methods/hazard_curve.R")
do.call(gridExtra::grid.arrange, HMD_UK_result_1_year_list)

#############
# Data

# show map of London with sites
source("./chap_data/London_places.R")
suppressWarnings(print(London_map))

############
# Results

# London population
source("./chap_results/London_population.R")
grid::grid.newpage()
grid::grid.draw(rbind(london_pop1, london_pop2))

# Simulation of population increase
source("./chap_results/simulations_pop_incr_run.R")
do.call(gridExtra::grid.arrange, c(lt_sim_plot_list, ncol = 4) )

## Historical Data
# Written sources, pre-processed
source("./chap_results/historical_lifetables.R")
peers_ranges
monks_ranges
london_1728_1840_ranges
london_1728_1840_ranges_r
London_1841_ranges
eng_mort_ranges
HMD_UK_ranges

# Wellcome Data
source("./lifetables_processing/stbrides_crypt.R")
source("./chap_results/Wellcome_DB.R") # can take a while
# St. Bride's crypt data, comparison of known age and osteological estimates
gridExtra::grid.arrange(stbrides_crypt_plot,
                        bottom = paste ("black = density of actual ages (bandwidth = 5)",
                        "blue = Gompertz distribution of actual ages",
                        "red = Gompertz distribution of osteological estimates",
                        sep="\n"))

# show overview of Wellcome data
wellcome_overview_all
write.table(wellcome_overview_all, file = "./documented/table_wellcome.txt", sep="\t", quote = FALSE)

# show ranges for St. Marylebone
source("./lifetables_processing/Marylebone.R")
Marylebone_ranges

############
# Discussion

# modal ages from historical and osteological data
source("./english_wellcome.R")
grid::grid.newpage()
grid::grid.draw(rbind(english_wellcome_plot, english_wellcome_plot_r))

##############
# Supplement

# one complete Bayesian example, with different settings
set.seed(1312)
source("./bayes_complete.R") # can take a few minutes
bayes_complete_table

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

# Written sources, pre-processed
source("./historical_lifetables.R")
peers_result
Landers_result
london_1728_1840_result
london_1728_1840_result_r
London_1841_result
eng_mort_result
HMD_UK_result
monks_result

# Mortality in the Wellcome dataset, pre-processed
source("./lifetables_processing/stbrides_crypt.R")
source("./Wellcome_DB.R")
wellcome_result
wellcome_result_r


##### out-dated code

# # computing the inaccuracy of resampled "age estimations"
# if (runCodeNew) {
#   lt_bias <- lt.sampling(1000, n_min = 50, n_max = 500, b_min = 0.02, b_max = 0.1, error_range = 15)
#   save(lt_bias, file = file.path(".", saveFileDir, "lt_bias.Rdata"))  
# } else {
#   load(file = file.path(".", saveFileDir, "lt_bias.Rdata"))  
# }
# mean(lt_bias$lt_inaccuracy)