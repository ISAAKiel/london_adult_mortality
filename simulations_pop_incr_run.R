if (runCodeNew){
  set.seed(3674)
  lt_sim_list <- list()
  for(k in 1:6) {
    lt_sim <- lt.MC.Gomp(pop_start = 1000, pop_inc = c(0, 0.005, 0.01, 0.02), years = 200,   
                         obs_start = 100, obs_end = 200, beta = (k + 1)/100)
    lt_sim$pop_inc_fac <- as.factor(lt_sim$pop_inc)
    lt_sim_list[[k]] <-   ggplot(lt_sim, aes(y = surv_Gompertz_shape, x = pop_inc_fac) ) + 
      geom_boxplot()  + 
      ylab("Gompertz \u03B2") + ggtitle(paste0("Original Gompertz \u03B2: ", (k + 1)/100) ) +
      xlab("population increase")
    lt_sim_list[[k + 6]] <-   ggplot(lt_sim, aes(y = 1 / surv_Gompertz_shape * log(surv_Gompertz_shape/surv_Gompertz_rate) + 15, x = pop_inc_fac) ) + 
      geom_boxplot()  + 
      ylab("modal age M") + ggtitle(paste0("Original Gompertz \u03B2: ", (k + 1)/100) ) +
      xlab("population increase")
  }
  
  # saves results in Rda-object
  save(lt_sim_list, file = file.path(".", saveFileDir, "lt_sim_list.Rda") )
}
load(file.path(".", saveFileDir, "lt_sim_list.Rda") )