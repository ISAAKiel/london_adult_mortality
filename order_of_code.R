#######################################################
# Prerequisites
# Install required packages
require(pacman) || install.packages("pacman")
pacman::p_load(coda, cowplot, demogR, dplyr, fitdistrplus, flexsurv, ggplot2, 
               ggrepel, gridExtra, HMDHFDplus, kableExtra, Metrics, mortAAR, 
               MortalityLaws, nlme, osmdata, pals, psych, readxl, reshape2, rgdal, 
               rio, rjags, rnaturalearth, runjags, sf, svMisc, tibble, tidyr, flexsurv, ggspatial)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

source("./functions/bayes_cat_poisson.R")
source("./functions/gomp_MLE.R")
source("./functions/gomp_MLE_adapted.R")
source("./functions/gomp_MLE_interval.R")
source("./functions/gomp_anthr_age.R")
source("./functions/gomp_anthr_age_r.R")
source("./functions/gomp_known_age.R")
source("./functions/gomp_known_age_r.R")
source("./functions/helper_functions.R")
source("./functions/lt_MC.R")
source("./functions/lt_MC_Gomp.R")

RNGkind("L'Ecuyer-CMRG") # conservative random number generator to avoid periodicity

# run extensive code anew. Set TRUE to run extensive code (6 h +)
runCodeNew <- FALSE
#runCodeNew <- TRUE

# ask for credentials of the Human Mortality Database if the code runs anew
if (runCodeNew){
  HMD_username <- readline(prompt = "Enter username: ")
  tstHMD_password <- readline(prompt="Enter password: ")
  credentials <- c(HMD_username, HMD_password)
}

# Specify filename prefix for saved files and create a folder if needed:
saveFileDir = "preprocessed_files"
if (saveFileDir %in% list.files(getwd())) {
  # Dir exists
}else{
  dir.create(file.path(".", saveFileDir), showWarnings = FALSE )
}

#############
# Introduction

# Figure 1: Gompertz.
source("./chapter_01_introduction/gompertz_distribution.R")
gridExtra::grid.arrange(gompertz_plot)

#############
# Materials and methods

# figure 3: Hazard curve (mx) to show turning point
source("./chapter_02_materials_and_methods/hazard_curve.R")
gridExtra::grid.arrange(HMD_UK_hazard_plot)

#############
# Data

# figure 4: Map of London with sites
source("./chapter_03_data/London_places.R")
suppressWarnings(print(London_map))

# figure 5 London population
source("./chapter_03_data/London_population.R")
grid::grid.newpage()
grid::grid.draw(rbind(london_pop1, london_pop2))

# footnote 6: Re-calculation of rates for Razzell/Spence 2007
# Calculated in ./chapter_03_data/London_population.R
razz_df

############
# Results

# Written sources, pre-processed
source("./chapter_04_results/historical_lifetables.R")
peers_ranges
monks_ranges
london_1728_1840_ranges
london_1728_1840_ranges_r
London_1841_ranges
eng_mort_ranges
HMD_UK_ranges

# Ranges for St. Marylebone with correction for population increase
source("./lifetables_processing/Marylebone.R")
Marylebone_ranges

# Extended results for written sources, pre-processed
source("./chapter_04_results/historical_lifetables.R")
peers_result
london_1728_1840_result
london_1728_1840_result_r
London_1841_result
eng_mort_result
HMD_UK_result
monks_result

# Extended results for London cemeteries, pre-processed
source("./lifetables_processing/stbrides_crypt.R")
source("./chapter_04_results/Wellcome_DB.R")
wellcome_result
wellcome_result_r

# figure 6: Modal ages from historical and osteological data
source("./chapter_04_results/english_wellcome.R")
plot(modal_ages_plot)

# table 2: Overview of modelled osteological data from London cemeteries
wellcome_overview_all
write.table(wellcome_overview_all, file = "./documented/table_wellcome.txt", sep="\t", quote = FALSE)

# figure 7: St. Bride's Crypt 
source("./lifetables_processing/stbrides_crypt.R")
source("./chapter_04_results/Wellcome_DB.R") # can take a while
# St. Bride's crypt data, comparison of known age and osteological estimates
stbrides_crypt_plot

# figure 8: Simulation of population increase
source("./chapter_04_results/simulations_pop_incr_run.R")
do.call(gridExtra::grid.arrange, c(lt_sim_plot_list, ncol = 4) )


############
# Supplements

# Minimum Gompertz beta in Coale/Demeny-Tables
source("./chapter_supplement/coale_demeny_life_tables_gompertz.R")
min(gompertz_df$Gompertz_shape)

# Simulations for evaluation of algorithms for retrieving Gompertz parameters
source("./chapter_supplement/simulations_run.R")
# plot of results of methods with known age-at-death
do.call(gridExtra::grid.arrange, c(plot_list_shapes, ncol = 3))

# plot of difference between expected and observed value
do.call(gridExtra::grid.arrange, c(plot_list_diff, ncol = 3))

# table of RMSEs
rmse_result[order(rmse_result$RMSE) ,]

# plot of results of methods with estimated age-at-death
do.call(gridExtra::grid.arrange, plot_list_estim_shapes)

# table of RMSEs
rmse_estim_result[order(rmse_estim_result$RMSE) ,]

# plot for Bayesian model of difference
do.call(gridExtra::grid.arrange, c(plot_list_bayes_diff, ncol = 2) )

# Show that means are stable in Bayesian modelling
source("./chapter_supplement/bayes_complete.R") # can take a few minutes
bayes_complete_table

##### out-dated code

# share of decrease in mortality for population growth
#source("./chap_discussion/ex15_increase.R")
#explain_sum

# # computing the inaccuracy of resampled "age estimations"
# if (runCodeNew) {
#   lt_bias <- lt.sampling(1000, n_min = 50, n_max = 500, b_min = 0.02, b_max = 0.1, error_range = 15)
#   save(lt_bias, file = file.path(".", saveFileDir, "lt_bias.Rdata"))  
# } else {
#   load(file = file.path(".", saveFileDir, "lt_bias.Rdata"))  
# }
# mean(lt_bias$lt_inaccuracy)