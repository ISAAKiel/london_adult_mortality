#######################################################
#
# Depending on the hardware, the subsequent code 
# can run for several hours or even a few days.
#
#######################################################
# Prerequisites
# Install "Just Another Gibbs Sampler" (JAGS) if you want to run the Bayesian analyses anew.
# Version 4.3 - as used here - can be downloaded in pre-compiled form for a number of OS here:
# https://sourceforge.net/projects/mcmc-jags/
# The manual can be found here: https://people.stat.sc.edu/hansont/stat740/jags_user_manual.pdf

# Install required packages
require(pacman) || install.packages("pacman")
pacman::p_load(coda, cowplot, demogR, dplyr, flexsurv, ggplot2, ggrepel, grid,
               gridExtra, HMDHFDplus, kableExtra, Metrics, mortAAR, osmdata, 
               psych, readxl, reshape2, rjags, runjags, sf, tidyr, ggspatial)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

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
  HMD_password <- readline(prompt="Enter password: ")
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

# Simulations for evaluation of algorithms for retrieving Gompertz parameters
# Subsequent Bayes calculations can take a while
source("./chapter_supplement/simulations_run.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
# figure 6: Plot of results of methods with known age-at-death
do.call(gridExtra::grid.arrange, c(plot_list_shapes, ncol = 3))

# figure 7: Plot of difference between observed and estimated value
do.call(gridExtra::grid.arrange, c(plot_list_diff, ncol = 3))

# table 3: RMSEs for known ages
rmse_result[order(rmse_result$RMSE) ,]

# figure 8: Plot of results of algorithms with estimated age-at-death
do.call(gridExtra::grid.arrange, plot_list_estim_shapes)

# table 4: RMSEs for estimated ages
rmse_estim_result[order(rmse_estim_result$RMSE) ,]

# figure 9: Simulation of population increase
source("./chapter_04_results/simulations_pop_incr_run.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
do.call(gridExtra::grid.arrange, c(lt_sim_plot_list, ncol = 4) )

# Written sources, pre-processed
# Subsequent Bayes calculations can take a while
warning("Subsequent sourcing of Bayes calculations. Progressbars will show up.")
source("./chapter_04_results/historical_lifetables.R")
london_1728_1840_ranges
london_1728_1840_ranges_r
London_1841_ranges
eng_mort_ranges
HMD_UK_ranges
peers_ranges
monks_ranges

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
# Subsequent Bayes calculations can take a while

# Subsequent code will ask for the file ST.BRIDE'S_CRYPT_Data 
# from the Museum of London.

# file.chose() doesn't open to the foreground!

source("./lifetables_processing/stbrides_crypt.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
source("./chapter_04_results/Wellcome_DB.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
wellcome_result
wellcome_result_r

# figure 10: Modal ages from historical and osteological data
source("./chapter_04_results/english_wellcome.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))

# table 5: Overview of modelled osteological data from London cemeteries
wellcome_overview_all
write.table(wellcome_overview_all, file = "./documented/table02_osteological_estimates.txt", sep="\t", quote = FALSE)

# figure 11: St. Bride's Crypt
# The following calculations have already been sourced before.
# Only run again if necessary or continue with 'stbrides_crypt_plot'.
source("./lifetables_processing/stbrides_crypt.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue"))
source("./chapter_04_results/Wellcome_DB.R", echo=TRUE,
       max.deparse.length=10000, continue.echo = getOption("continue")) 
# St. Bride's crypt data, comparison of known age and osteological estimates
stbrides_crypt_plot


############
# Supplements

# Minimum Gompertz beta in Coale/Demeny-Tables
source("./chapter_supplement/coale_demeny_life_tables_gompertz.R")
min(gompertz_df$Gompertz_shape)

# Figure S.F1.: Plot for Bayesian model of difference
do.call(gridExtra::grid.arrange, c(plot_list_bayes_diff, ncol = 2) )

# Table S.T1: Show that means are stable in Bayesian modelling
# can take a few minutes
source("./chapter_supplement/bayes_complete.R",
       max.deparse.length=10000, continue.echo = getOption("continue"))
bayes_complete_table