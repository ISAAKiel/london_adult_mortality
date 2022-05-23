gridExtra::grid.arrange (
  ggplot(lt_sim) + geom_point(aes(x = b_, y = surv_Gompertz_shape), shape=1) + xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("known Gompertz \u03B2") + ylab("estimated Gompertz \u03B2"),
  ggplot(lt_sim) + geom_point(aes(x = b_, y = surv_lt_Gompertz_shape), shape=1) + xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("known Gompertz \u03B2") + ylab("estimated Gompertz \u03B2"),
  ggplot(lt_sim) + geom_point(aes(x = b_, y = surv_lt10_Gompertz_shape), shape=1) + xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("known Gompertz \u03B2") + ylab("estimated Gompertz \u03B2"),
  ggplot(lt_sim) + geom_point(aes(x = b_, y = WOLS_Gompertz_shape), shape=1) + xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("known Gompertz \u03B2") + ylab("estimated Gompertz \u03B2"),
  ggplot(lt_sim) + geom_point(aes(x = b_, y = NLS_Gompertz_shape), shape=1) + xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("known Gompertz \u03B2") + ylab("estimated Gompertz \u03B2"),
  ggplot(lt_sim) + geom_point(aes(x = b_, y = bayes_gomp_b), shape=1) + xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("known Gompertz \u03B2") + ylab("estimated Gompertz \u03B2"),
  ggplot(lt_sim) + geom_point(aes(x = b_, y = OLS_Gompertz_shape), shape=1) + xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("known Gompertz \u03B2") + ylab("estimated Gompertz \u03B2"),
  ncol = 3
)

gridExtra::grid.arrange (
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - surv_Gompertz_shape)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - surv_lt10_Gompertz_shape)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - WOLS_Gompertz_shape)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - NLS_Gompertz_shape)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - bayes_gomp_b)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ncol = 3
)


ggplot(lt_sim) + geom_point(aes(x = b_, y = LF2_Gompertz_shape)) 
ggplot(lt_sim) + geom_point(aes(x = b_, y = poisson_Gompertz_shape))
ggplot(lt_sim) + geom_point(aes(x = b_, y = binom_Gompertz_shape))
ggplot(lt_sim) + geom_point(aes(x = b_, y = NLS_Gompertz_shape_qx)) + xlim(0.02, 0.1) + ylim(0.02, 0.1)

ggplot(lt_sim) + geom_point(aes(x = surv_Gompertz_shape, y = WOLS_Gompertz_shape))
ggplot(lt_sim) + geom_point(aes(x = LF2_Gompertz_shape, y = WOLS_Gompertz_shape))

fit <- lm(surv_Gompertz_shape ~ b_ , data = lt_sim) #best fit
summary(fit)
fit <- lm(WOLS_Gompertz_shape ~ b_ , data = lt_sim) #fourth best fit, underestimates beta
summary(fit)
fit <- lm(OLS_Gompertz_shape ~ b_ , data = lt_sim) #fourth best fit, underestimates beta
summary(fit)
fit <- lm(LF2_Gompertz_shape ~ b_ , data = lt_sim)
summary(fit)
fit <- lm(poisson_Gompertz_shape ~ b_ , data = lt_sim)
summary(fit)
fit <- lm(surv_lt_Gompertz_shape ~ b_ , data = lt_sim) # second best
summary(fit)
fit <- lm(NLS_Gompertz_shape ~ b_ , data = lt_sim)# third best
summary(fit)
fit <- lm(bayes_gomp_b ~ b_ , data = lt_sim)
summary(fit)


betas <- c("surv_Gompertz_shape", "WOLS_Gompertz_shape", "surv_lt10_Gompertz_shape","OLS_Gompertz_shape",
           # "LF2_Gompertz_shape", "poisson_Gompertz_shape", "binom_Gompertz_shape", 
           "NLS_Gompertz_shape", "bayes_gomp_b")
betas <- c("WOLS_estim_Gompertz_shape", #"surv_estim_lt_Gompertz_shape", 
           "NLS_estim_Gompertz_shape", "bayes_anthr_gomp_b")
rmse_result <- NULL
for (t in betas) {
  beta_exp <- lt_sim$b_
  beta_obs <- lt_sim[t]
  betas_comb <- na.omit(data.frame(beta_exp, beta_obs))
  colnames(betas_comb) <- c("beta_exp", "beta_obs")
  rmse_result[t] <- Metrics::rmse(betas_comb$beta_exp, betas_comb$beta_obs)
}
rmse_result <- data.frame(betas, rmse_result)
rownames(rmse_result) <- NULL
rmse_result[order(rmse_result$rmse_result) ,]

# age estimations
gridExtra::grid.arrange (
ggplot(lt_sim) + geom_point(aes(x = b_, y = WOLS_estim_Gompertz_shape)) + xlim(0.015, 0.2) + ylim(0.015, 0.1),
ggplot(lt_sim) + geom_point(aes(x = b_, y = surv_estim_lt_Gompertz_shape)) + xlim(0.015, 0.1) + ylim(0.015, 0.6),
ggplot(lt_sim) + geom_point(aes(x = b_, y = NLS_estim_Gompertz_shape)) + xlim(0.015, 0.1) + ylim(0.015, 0.1),
ggplot(lt_sim) + geom_point(aes(x = b_, y = bayes_anthr_gomp_b)) + xlim(0.015, 0.1) + ylim(0.015, 0.1),
ncol = 2
)

gridExtra::grid.arrange (
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - WOLS_estim_Gompertz_shape)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - surv_estim_lt_Gompertz_shape)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - NLS_estim_Gompertz_shape)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ggplot(lt_sim) + geom_point(aes(x = y, y = (b_ - bayes_anthr_gomp_b)), shape=1) + #xlim(0.02, 0.1) + ylim(0.02, 0.1) + 
    xlab("sample size") + ylab("difference"),
  ncol = 2
)

fit <- lm(WOLS_estim_Gompertz_shape ~ b_ , data = lt_sim)
summary(fit)
fit <- lm(bayes_anthr_gomp_b ~ b_ , data = lt_sim)
summary(fit)
