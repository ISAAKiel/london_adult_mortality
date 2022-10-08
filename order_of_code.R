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
#runCodeNew <- FALSE
runCodeNew <- TRUE

# Specify filename prefix for saved files and create a folder if needed:
saveFileDir = "preprocessed_files"
if (saveFileDir %in% list.files(getwd())) 
{}else{
  dir.create(file.path(".", saveFileDir), showWarnings = FALSE )
}

#############
# Methods

# Figure 1: Gompertz.
source("./gompertz_distribution.R")

# Minimum Gompertz beta in Coale/Demeny-Tables
source("./lifetables_processing/coale_demeny_life_tables_gompertz.R")
min(gompertz_df$Gompertz_shape)

# hazard curve (mx) to show turning point
source("./hazard_curve.R")
do.call(gridExtra::grid.arrange, HMD_UK_result_1_year_list)

#############
# Data

# computing the inaccuracy of resampled "age estimations"
if (runCodeNew) {
  lt_bias <- lt.sampling(1000, n_min = 50, n_max = 500, b_min = 0.02, b_max = 0.1, error_range = 15)
  save(lt_bias, file = file.path("..", saveFileDir, "lt_bias.Rdata"))  
} else {
  load(file = file.path("..", saveFileDir, "lt_bias.Rdata"))  
}
mean(lt_bias$lt_inaccuracy)

# show map of London with sites
source("./London_places.R") # not working yet


############
# Results

# London population
source("./London_population.R")
do.call(gridExtra::grid.arrange, c(london_pop_list, ncol = 1) )

# Simulation of population increase
source("./simulations_pop_incr_run.R")
do.call(gridExtra::grid.arrange, c(lt_sim_list, ncol = 6) )

## Historical Data
# Written sources, pre-processed
source("./historical_lifetables.R")
Peers_ranges
monks_ranges
London_1758_ranges
London_1841_ranges
eng_mort_ranges
HMD_UK_ranges

# Wellcome Data
source("./lifetables_processing/stbrides_crypt.R")
source("./lifetables_processing/Merton_Priory.R")
source("./Wellcome_DB.R") # can take a while
# St. Bride's crypt data, comparison of known age and osteological estimates
gridExtra::grid.arrange(stbrides_crypt_plot,
                        bottom = "black = density of actual ages (bandwidth = 5)\n blue = Gompertz distribution of actual ages\n red = Gompertz distribution of osteological estimates")

# show overview of Wellcome data
wellcome_overview 
# to do: sample size


############
# Discussion

# modal ages from historical and osteological data
english_wellcome <- rbind(english_mortality_prep, wellcome_prep)
english_wellcome$data <- factor(english_wellcome$data, levels = unique(english_wellcome$data))

english_wellcome_plot <- ggplot(english_wellcome, aes(colour = data, shape = source) ) +  
    ylab("modal age")  + xlab("year") + ylim(10, 70) +
  geom_errorbar(aes(x = (start + end) / 2, y = M, ymin = HDIlow, ymax=  HDIhigh), width=0, colour = "dark grey") +
  geom_errorbarh(aes(x = (start + end) / 2, y = M, xmax = start, xmin = end, height = 0), colour = "dark grey") +
  geom_point(aes(x = as.numeric(substr(year, 2, 5)), y = M), size= 3 )+ 
  geom_point(aes(x = (start + end) / 2, y = M), size= 3) + guides(size = "none")
suppressWarnings(print(english_wellcome_plot))

# Gompertz beta from historical and osteological data
english_wellcome_beta <- rbind(english_mortality_beta_prep, wellcome_prep_beta)
english_wellcome_beta$data <- factor(english_wellcome_beta$data, levels = unique(english_wellcome_beta$data))

english_wellcome_plot_beta <- ggplot(english_wellcome_beta, aes(colour = data, shape = source) ) +  
  ylab("Gompertz \u03B2")  + xlab("year") + #ylim(10, 70) +
  geom_errorbar(aes(x = (start + end) / 2, y = beta, ymin = HDIlow, ymax=  HDIhigh), width=0, colour = "dark grey") +
  geom_errorbarh(aes(x = (start + end) / 2, y = beta, xmax = start, xmin = end, height = 0), colour = "dark grey") +
  geom_point(aes(x = as.numeric(substr(year, 2, 5)), y = beta), size= 3 )+ 
  geom_point(aes(x = (start + end) / 2, y = beta), size= 3) + guides(size = "none")
suppressWarnings(print(english_wellcome_plot_beta))

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
Peers_result
Landers_result
London_1758_result
London_1841_result
eng_mort_result
HMD_UK_result
monks_result

# Mortality in the Wellcome dataset, pre-processed
source("./lifetables_processing/stbrides_crypt.R")
source("./lifetables_processing/Merton_Priory.R")
source("./Wellcome_DB.R")
wellcome_result
