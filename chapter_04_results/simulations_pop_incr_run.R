if (runCodeNew){
  set.seed(3673)
  lt_sim_list <- list()
  for(k in 1:4) {
    lt_sim <- lt.MC.Gomp(pop_start = c(10000, 1000, 500, 200, 100), 
                         pop_inc = c(-0.02, 0, 0.005, 0.01, 0.02), 
                         years = 200,   
                         obs_start = 150, 
                         obs_end = 200, 
                         beta = (k + 2)/100, 
                         bayes = TRUE)
    lt_sim_list[[k]] <- lt_sim
  }
  
  # saves results in Rda-object
  save(lt_sim_list, file = file.path(".", saveFileDir, "lt_sim_list.Rda") )
}
load(file.path(".", saveFileDir, "lt_sim_list.Rda") )

lt_sim_plot_list <- list()
for (i in 1:4) {
lt_sim_plot_list[[i]] <- ggplot(lt_sim_list[[i]], aes(y = surv_Gompertz_shape, x = as.factor(pop_inc))) + 
  geom_boxplot()  + 
  ggtitle(paste0("\u03B2: ", (i + 2)/100) ) +
  ylab("Gompertz \u03B2 (MLE)") + xlab("population increase") + 
  theme(plot.margin = unit(c(0,0.5,0.5,0), "cm"),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8)) + theme_light()
lt_sim_plot_list[[i + 4]] <-   ggplot(lt_sim_list[[i]], aes(y = bayes_gomp_b, x = as.factor(pop_inc)) ) + 
  geom_boxplot()  + 
  ggtitle(paste0("\u03B2: ", (i + 2)/100) ) +
  ylab("Gompertz \u03B2 (Bayes)") + xlab("population increase") + 
  theme(plot.margin = unit(c(0,0.5,0.5,0), "cm"),
              plot.title = element_text(size = 12),
              axis.title = element_text(size = 10),
              axis.text = element_text(size = 8)) + theme_light()
}

lt_sim_plots <- gridExtra::grid.arrange (grobs = lt_sim_plot_list, ncol = 4,
                                         top = textGrob("Original Gompertz\n", 
                                                        gp=gpar(fonsize = 14)))
# Save the finished map object
ggsave(
  filename = "fig08_lt_sim_plots.pdf",
  width = 11.5, height = 8,
  plot = lt_sim_plots, 
  device = cairo_pdf,
  path = "documented"
)
