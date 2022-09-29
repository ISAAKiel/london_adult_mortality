# Global History of Health samples
source("./BoE_data.R")
# table of individuals and samples of Global History of Health
BoE_table_ind
BoE_table_sites
source("./BoE_map.R") # download of data from naturalearthdata
BoE_map


# Historic life tables
source("./historical_lifetables.R")
do.call(gridExtra::grid.arrange, c(hist_lt, ncol = 3))
do.call(gridExtra::grid.arrange, c(germany_list, ncol = 2))

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


# plot bad age diagrams of BoE
do.call(gridExtra::grid.arrange, c(plot_list_bad, ncol = 3))
# plot 9 randomly selected good age diagrams
do.call(gridExtra::grid.arrange, c(plot_list_good, ncol = 3))