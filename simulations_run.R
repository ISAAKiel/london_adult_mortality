if (runCodeNew){
set.seed(209)
lt_sim <- lt.MC(sampling = 1000,
                n_min = 50,
                n_max = 500,
                #M_min = 25,
                #M_max = 80,
                b_min = 0.015,
                b_max = 0.1,
                error_range = 15,
                age_categories = "BoE",
                bayes = TRUE,
                thinSteps = 1,
                numSavedSteps = 10000
)

# saves results in Rda-object
save(lt_sim, file = file.path(".", saveFileDir, "lt_sim.Rda") )
}
load(file.path(".", saveFileDir, "lt_sim.Rda") )

lt_sim_shapes <- c("surv_Gompertz_shape", "surv_lt_Gompertz_shape", "surv_lt10_Gompertz_shape",
                   "OLS_Gompertz_shape", "WOLS_Gompertz_shape", "NLS_Gompertz_shape",
                   "MLE_lt_Gompertz_shape", "bayes_gomp_b")
lt_sim_shapes_names <- c("survival", "survival (5y-cat)", "survival (10y-cat)", 
                         "OLS", "WOLS", "WNLS", "MLE", "Bayes")
lt_sim_estim_shapes <- c("surv_estim_lt_Gompertz_shape", "WOLS_estim_Gompertz_shape", "OLS_estim_Gompertz_shape",
                         "NLS_estim_Gompertz_shape", "MLE_estim_lt_Gompertz_shape", "bayes_anthr_gomp_b")
lt_sim_estim_shapes_names <- c("survival", "WOLS", "OLS", "WNLS", "MLE", "Bayes")

plot_list_shapes <- list()
for (i in 1:length(lt_sim_shapes)) {
  plot_df <- data.frame(beta_original = lt_sim$b_, beta = lt_sim[,lt_sim_shapes[i]])
  plot_list_shapes[[i]] <- ggplot(plot_df, aes(x = beta_original, y = beta)) + geom_point(shape = 21) + 
     xlab("original \u03B2") + ylab("estimated \u03B2") + ggtitle(lt_sim_shapes_names[i]) + xlim(0.01, 0.105) + ylim(0.01, 0.105)
}

plot_list_diff <- list()
for (i in 1:length(lt_sim_shapes)) {
  plot_df <- data.frame(beta_original = lt_sim$b_, beta = lt_sim[,lt_sim_shapes[i]])
  plot_list_diff[[i]] <- ggplot(plot_df, aes(x = beta_original, y = beta_original - beta)) + geom_point(shape = 21) + 
    xlab("original \u03B2") + ylab("estimated \u03B2") + ggtitle(lt_sim_shapes_names[i]) + xlim(0.01, 0.105) + ylim(0.02, -0.04)
}

rmse_result <- NULL
NAs <- NULL
for (t in lt_sim_shapes) {
  beta_exp <- lt_sim$b_
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
  plot_df <- data.frame(beta_original = lt_sim$b_, beta = lt_sim[,lt_sim_estim_shapes[i]])
  plot_list_estim_shapes[[i]] <- ggplot(plot_df, aes(x = beta_original, y = beta)) + 
    geom_point(shape = 21) + xlab("original \u03B2") + ylab("estimated \u03B2") + 
    ggtitle(lt_sim_estim_shapes_names[i]) + xlim(0.01, 0.105) + ylim(0.01, 0.105)
}

rmse_estim_result <- NULL
NAs <- NULL
for (t in lt_sim_estim_shapes) {
  beta_exp <- lt_sim$b_
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
MLE_wo_OL <- Metrics::rmse(lt_sim_sub$b_, lt_sim_sub$MLE_estim_lt_Gompertz_shape)
MLE_wo_OL_count <- 1000 - count(lt_sim_sub)
MLE_wo_OL_df <- data.frame("MLE_wo_OL", MLE_wo_OL, MLE_wo_OL_count)
colnames(MLE_wo_OL_df) <- c("method", "RMSE", "NAs")
rmse_estim_result <- rbind(rmse_estim_result,MLE_wo_OL_df)


plot_list_bayes_diff <- list(
  ggplot(lt_sim, aes(x = b_, y = b_ - bayes_anthr_gomp_b )) + geom_point(shape = 21) +
    xlab("original \u03B2") + ylab("original \u03B2 - estimated \u03B2"),
  ggplot(lt_sim, aes(x = y, y = b_ - bayes_anthr_gomp_b )) + geom_point(shape = 21) +
    xlab("sample size") + ylab("original \u03B2 - estimated \u03B2")
)

ggplot(lt_sim) + geom_boxplot(aes(x = cut_interval(b_, length = 0.01),  y = surv_Gompertz_shape)) +
  scale_y_continuous(breaks = round(seq(min(lt_sim$surv_Gompertz_shape), max(lt_sim$surv_Gompertz_shape), by = 0.01),2))
ggplot(lt_sim) + geom_boxplot(aes(x = cut_interval(b_, length = 0.01),  y = MLE_lt_Gompertz_shape)) +
  scale_y_continuous(breaks = round(seq(min(lt_sim$MLE_lt_Gompertz_shape), max(lt_sim$MLE_lt_Gompertz_shape), by = 0.01),2))
ggplot(lt_sim) + geom_boxplot(aes(x = cut_interval(b_, length = 0.01),  y = bayes_gomp_b)) +
  scale_y_continuous(breaks = round(seq(min(lt_sim$bayes_gomp_b), max(lt_sim$bayes_gomp_b), by = 0.01),2))
