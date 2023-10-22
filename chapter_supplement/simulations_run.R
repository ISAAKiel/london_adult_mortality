if (runCodeNew){
  set.seed(6833)
  lt_sim <- lt.MC(sampling = 1000,
                  n_min = 50,
                  n_max = 500,
                  b_min = 0.02,
                  b_max = 0.1,
                  thinSteps = 1,
                  numSavedSteps = 10000
  )
  
  # saves results in Rda-object
  save(lt_sim, file = file.path(".", saveFileDir, "lt_sim.Rda") )
}
load(file.path(".", saveFileDir, "lt_sim.Rda") )

lt_sim_shapes <- c("OLS_Gompertz_shape", "WOLS_Gompertz_shape", "NLS_Gompertz_shape",
                   "surv_Gompertz_shape", "surv_lt_Gompertz_shape", "surv_lt10_Gompertz_shape",
                   "MLE_adapted_Gompertz_shape", "MLE_lt_Gompertz_shape", "MLE_lt10_Gompertz_shape",
                   "bayes_gomp_b", "bayes_anthr_gomp_5y_b", "bayes_anthr_gomp_10y_b",
                   "bayes_poisson_b")
lt_sim_shapes_names <- c("OLS", "WOLS", "WNLS",  
                         "survival", "survival (5y-cat)", "survival (10y-cat)",
                         "MLE", "MLE (5y-cat)", "MLE (10y-cat)",
                         "Bayes", "Bayes (5y-cat)", "Bayes (10y-cat)", "Bayes poisson")
lt_sim_estim_shapes <- c("OLS_estim_Gompertz_shape", "WOLS_estim_Gompertz_shape","NLS_estim_Gompertz_shape",  
                         "surv_estim_lt_Gompertz_shape", "MLE_estim_lt_Gompertz_shape", "bayes_anthr_gomp_b",
                         "bayes_estim_poisson_b")
lt_sim_estim_shapes_names <- c("OLS", "WOLS", "WNLS", "survival (cat)", "MLE (cat)", "Bayes (cat)",
                               "Bayes poisson")

plot_list_shapes <- list()
for (i in 1:length(lt_sim_shapes)) {
  plot_df <- data.frame(beta_original = lt_sim$beta, beta = lt_sim[,lt_sim_shapes[i]])
  plot_list_shapes[[i]] <- ggplot(plot_df, aes(x = beta_original, y = beta)) + geom_point(shape = 21) + 
    xlab("original \u03B2") + ylab("estimated \u03B2") + ggtitle(lt_sim_shapes_names[i]) + 
    xlim(0.01, 0.105) + ylim(0.01, 0.105) + theme_light()
}

plot_list_diff <- list()
for (i in 1:length(lt_sim_shapes)) {
  plot_df <- data.frame(beta_original = lt_sim$beta, beta = lt_sim[,lt_sim_shapes[i]])
  plot_list_diff[[i]] <- ggplot(plot_df, aes(x = beta_original, y = beta_original - beta)) + geom_point(shape = 21) + 
    xlab("original \u03B2") + ylab("estimated \u03B2") + ggtitle(lt_sim_shapes_names[i]) + 
    xlim(0.01, 0.105) + ylim(0.02, -0.04) + theme_light()
}

rmse_result <- NULL
NAs <- NULL
for (t in lt_sim_shapes) {
  beta_exp <- lt_sim$beta
  beta_obs <- lt_sim[t]
  betas_comb <- na.omit(data.frame(beta_exp, beta_obs))
  NAs[t] <- 1000 - nrow(betas_comb)
  colnames(betas_comb) <- c("beta_exp", "beta_obs")
  rmse_result[t] <- Metrics::rmse(betas_comb$beta_exp, betas_comb$beta_obs)
}
rmse_result <- data.frame(method = lt_sim_shapes_names, RMSE = rmse_result, NAs = NAs)
rownames(rmse_result) <- NULL

plot_list_estim_shapes <- list()
for (i in 1:length(lt_sim_estim_shapes)) {
  plot_df <- data.frame(beta_original = lt_sim$beta, beta = lt_sim[,lt_sim_estim_shapes[i]])
  plot_list_estim_shapes[[i]] <- ggplot(plot_df, aes(x = beta_original, y = beta)) + 
    geom_point(shape = 21) + xlab("original \u03B2") + ylab("estimated \u03B2") + 
    ggtitle(lt_sim_estim_shapes_names[i]) + xlim(0.01, 0.105) + ylim(0.01, 0.105)
}

rmse_estim_result <- NULL
NAs <- NULL
for (t in lt_sim_estim_shapes) {
  beta_exp <- lt_sim$beta
  beta_obs <- lt_sim[t]
  betas_comb <- na.omit(data.frame(beta_exp, beta_obs))
  NAs[t] <- 1000 - nrow(betas_comb)
  colnames(betas_comb) <- c("beta_exp", "beta_obs")
  rmse_estim_result[t] <- Metrics::rmse(betas_comb$beta_exp, betas_comb$beta_obs)
}
rmse_estim_result <- data.frame(lt_sim_estim_shapes_names, rmse_estim_result, NAs)
rownames(rmse_estim_result) <- NULL
colnames(rmse_estim_result) <- c("method", "RMSE", "NAs")

# computing RMSE by hand for extreme outliers of MLE
lt_sim_sub <- subset(lt_sim, MLE_estim_lt_Gompertz_shape < 0.15)
MLE_wo_OL <- Metrics::rmse(lt_sim_sub$beta, lt_sim_sub$MLE_estim_lt_Gompertz_shape)
MLE_wo_OL_count <- 1000 - count(lt_sim_sub)
MLE_wo_OL_df <- data.frame("MLE_wo_OL", MLE_wo_OL, MLE_wo_OL_count)
colnames(MLE_wo_OL_df) <- c("method", "RMSE", "NAs")
rmse_estim_result <- rbind(rmse_estim_result,MLE_wo_OL_df)

plot_list_bayes_diff <- list(
  ggplot(lt_sim, aes(x = beta, y = beta - bayes_anthr_gomp_b )) + geom_point(shape = 21) +
    xlab("original \u03B2") + ylab("original \u03B2 - estimated \u03B2") + 
    geom_smooth(method = "loess", formula = y ~ x),
  ggplot(lt_sim, aes(x = y, y = beta - bayes_anthr_gomp_b )) + geom_point(shape = 21) +
    xlab("sample size") + ylab("original \u03B2 - estimated \u03B2") + 
    geom_smooth(method = "loess", formula = y ~ x)
)