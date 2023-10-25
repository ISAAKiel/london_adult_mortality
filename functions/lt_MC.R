# this function produces a certain number of random sample populations with
# random Gompertz parameters and then fits these populations with different formulas
# to estimate the original parameters. Fitted are "known ages", life table data
# with 5- and 10-year age ranges as well as osteological age categories as used
# by Museum of London Archaeology
lt.MC <- function(sampling,
                  n_min = 50,
                  n_max = 500,
                  b_min = 0.025,
                  b_max = 0.1,
                  thinSteps = 1,
                  numSavedSteps = 10000 )
{
  start_time <- Sys.time()
  lt_result <- data.frame()
  for (g in 1:sampling) {
    # sampling with Gompertz distribution
    ind_df <- lt.sampling(sampling = 1, n_min = n_min, n_max = n_max, b_min = b_min, b_max = b_max)
    
    # fit Gompertz distribution to known age with Survival package + individual age
    ind_df$death <- 1
    ind_dfGomp <- flexsurv::flexsurvreg(formula = survival::Surv(age - 15, death) ~ 1, data = ind_df, dist="gompertz")
    surv_Gompertz_shape <- ind_dfGomp$coefficients[1]
    surv_Gompertz_rate <- exp(ind_dfGomp$coefficients[2])
    
    # # fit home-made MLE to individual age, original Konigsberg formula
    MLE_Gompertz_shape <- NA
    MLE_Gompertz_rate <- NA
    tryCatch({
      MLE_Gomp <- Gomp.MLE(ind_df, age = "age")
      MLE_Gompertz_shape <- MLE_Gomp[2]
      MLE_Gompertz_rate <- MLE_Gomp[1]
    }, error=function(e){})
    
    # # fit home-made MLE to individual age, adapted and simplified Konigsberg formula
    MLE_adapted_Gompertz_shape <- NA
    MLE_adapted_Gompertz_rate <- NA
    tryCatch({
      MLE_adapted_Gomp <- Gomp.MLE.adapted(ind_df, age = "age")
      MLE_adapted_Gompertz_shape <- MLE_adapted_Gomp[2]
      MLE_adapted_Gompertz_rate <- MLE_adapted_Gomp[1]
    }, error=function(e){})
    
    # compute life table with 5-year-classes, we cannot use mortAAR as it truncates ages above 99 years
    ind_df$x_vec <- floor((ind_df$age) / 5 ) * 5 - 15
    age_vec <- data.frame(x_vec = seq(0,floor(max(ind_df$age -15)), 5))
    mort_x <- ind_df %>% group_by (x_vec) %>% summarize(Dx_vec = n())
    mort_prep_x <- merge(age_vec, mort_x, all = TRUE)
    mort_prep_x[is.na(mort_prep_x)] <- 0
    Dx_vec <- mort_prep_x$Dx_vec
    x_vec <- mort_prep_x$x_vec
    length_mort <- length(x_vec)
    x_vec2 <- x_vec + 5
    dx_vec <- Dx_vec / sum(Dx_vec)
    # calculation of lx
    dx_ <- NULL
    lx_vec <- NULL
    Lx_vec <- NULL
    lx_ <- 1
    for (k in 1: (length_mort) ) {
      lx_vec <- c(lx_vec, lx_)
      lx_old <- lx_
      dx_ <- dx_vec[k]
      lx_ <- lx_ - dx_
      Lx_ <- 5 * (lx_old + lx_) / 2
      Lx_vec <- c(Lx_vec, Lx_)
    }
    qx_vec <- dx_vec / lx_vec
    mx_vec <- dx_vec / Lx_vec
    kx <- Dx_vec / ( mx_vec * 5 )
    mort_df_x <- data.frame(x_vec, x_vec2, Dx_vec, qx_vec, lx_vec, mx_vec, kx)
    
    # #fit OLS to life table mx
    OLS_Gompertz_shape <- NA
    OLS_Gompertz_rate <- NA
    tryCatch({
      mort_fit_OLS <- lm(log(mx_vec)  ~ x_vec, data = mort_df_x)
      OLS_Gompertz_shape <- mort_fit_OLS$coefficients[2]
      OLS_Gompertz_rate <- exp(mort_fit_OLS$coefficients[1])
    }, error=function(e){})
    
    #fit WOLS to life table mx
    mort_fit_WOLS <- lm(log(mx_vec)  ~ x_vec, data = mort_df_x, weights  = Dx_vec)
    WOLS_Gompertz_shape <- mort_fit_WOLS$coefficients[2]
    WOLS_Gompertz_rate <- exp(mort_fit_WOLS$coefficients[1])
    
    # fit survival data with nls via lx
    NLS_Gompertz_shape <- NA
    NLS_Gompertz_rate <- NA
    tryCatch({
      nls_fit <- nls(lx_vec ~ exp(a/b - a/b * exp(b * x_vec ) ) , 
                     data = mort_df_x, start=list(a = 0.001, b = 0.075), weights = Dx_vec)
      NLS_Gompertz_shape <- summary(nls_fit)$coefficients[2]
      NLS_Gompertz_rate <- summary(nls_fit)$coefficients[1]
    }, error=function(e){})    
    
    # # fit home-made MLE
    MLE_lt_Gompertz_shape <- NA
    MLE_lt_Gompertz_rate <- NA
    tryCatch({
      MLE_lt <- Gomp.MLE.interval(mort_df_x, agebegin = "x_vec", ageend = "x_vec2", Dx = "Dx_vec")
      MLE_lt_Gompertz_shape <- MLE_lt[2]
      MLE_lt_Gompertz_rate <- MLE_lt[1]
    }, error=function(e){})
    
    # #fit survival to life table Dx
    surv_lt_Gompertz_shape <- NA
    surv_lt_Gompertz_rate <- NA
    mort_df_x$death <- 1
    tryCatch({
      surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(x_vec, death) ~ 1, data = mort_df_x, dist="gompertz", weights = Dx_vec)
      surv_lt_Gompertz_shape <- surv_lt$coefficients[1]
      surv_lt_Gompertz_rate <- exp(surv_lt$coefficients[2])
    }, error=function(e){})
    
    # fit bayesian poisson model to lifetable data
    # we have to add 15, otherwise the minimum age setting will not work properly
    mort_df_x$x_vec_15 <- mort_df_x$x_vec + 15
    mort_df_x$x_vec2_15 <- mort_df_x$x_vec2 + 15
    poisson.interval(mort_df_x, age_beg = "x_vec_15", age_end = "x_vec2_15", Dx = "Dx_vec",
                       silent.jags = TRUE,
                       silent.runjags = TRUE,
                       thinSteps = 1,
                       numSavedSteps = 10000,
                       minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    bayes_poisson_b <- gomp_anthr_MCMC_diag[2,5]
    bayes_poisson_a <- gomp_anthr_MCMC_diag[1,5]
    
    #Bayes model from life table data, 5-year-interval
    mort_df_x_uncount <- mort_df_x %>% uncount(as.integer(Dx_vec))
    mort_df_x_uncount$x_vec_15 <-  mort_df_x_uncount$x_vec + 15 # we have to add 15, otherwise the minimum age setting will not work
    mort_df_x_uncount$x_vec2_15 <-  mort_df_x_uncount$x_vec2 + 15
    gomp.anthr_age(mort_df_x_uncount, age_beg = "x_vec_15", age_end = "x_vec2_15",
                   silent.jags = TRUE,
                   silent.runjags = TRUE,
                   thinSteps = thinSteps,
                   numSavedSteps = numSavedSteps,
                   minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    bayes_anthr_gomp_5y_b <- gomp_anthr_MCMC_diag[2,5]
    bayes_anthr_gomp_5y_a <- gomp_anthr_MCMC_diag[1,5]
    
    
    # create life table with 10-year age ranges
    ind_df$x_vec <- floor((ind_df$age + 5) / 10 ) * 10 - 20
    age_vec <- data.frame(x_vec = seq(0,floor(max(ind_df$age -15)), 10))
    mort_x <- ind_df %>% group_by (x_vec) %>% summarize(Dx_vec = n())
    mort_prep_x <- merge(age_vec, mort_x, all = TRUE)
    mort_prep_x[is.na(mort_prep_x)] <- 0
    x_vec <- mort_prep_x$x_vec
    x_vec2 <- x_vec + 10
    Dx_vec <- mort_prep_x$Dx_vec
    mort_df_x <- data.frame(x_vec, x_vec2, Dx_vec)
    
    # #fit survival to life table Dx
    surv_lt10_Gompertz_shape <- NA
    surv_lt10_Gompertz_rate <- NA
    mort_df_x$death <- 1
    tryCatch({
      surv_lt10 <- flexsurv::flexsurvreg(formula = survival::Surv(x_vec, death) ~ 1, data = mort_df_x, dist="gompertz", weights = Dx_vec)
      surv_lt10_Gompertz_shape <- surv_lt10$coefficients[1]
      surv_lt10_Gompertz_rate <- exp(surv_lt10$coefficients[2])
    }, error=function(e){})
    
    # # fit home-made MLE
    MLE_lt10_Gompertz_shape <- NA
    MLE_lt10_Gompertz_rate <- NA
    tryCatch({
      MLE_lt10 <- Gomp.MLE.interval(mort_df_x, agebegin = "x_vec", ageend = "x_vec2", Dx = "Dx_vec")
      MLE_lt10_Gompertz_shape <- MLE_lt10[2]
      MLE_lt10_Gompertz_rate <- MLE_lt10[1]
    }, error=function(e){})
    
    #Bayes model from life table data, 10-year-interval
    mort_df_x_uncount <- mort_df_x %>% uncount(as.integer(Dx_vec))
    mort_df_x_uncount$x_vec_15 <-  mort_df_x_uncount$x_vec + 15
    mort_df_x_uncount$x_vec2_15 <-  mort_df_x_uncount$x_vec2 + 15
    bayes_anthr_gomp_10y_b <- NA
    bayes_anthr_gomp_10y_a <- NA
    gomp.anthr_age(mort_df_x_uncount, age_beg = "x_vec_15", age_end = "x_vec2_15",
                   silent.jags = TRUE,
                   silent.runjags = TRUE,
                   thinSteps = thinSteps,
                   numSavedSteps = numSavedSteps,
                   minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    bayes_anthr_gomp_10y_b <- gomp_anthr_MCMC_diag[2,5]
    bayes_anthr_gomp_10y_a <- gomp_anthr_MCMC_diag[1,5]
    
    # Bayes model from individual data
    bayes_gomp_b <- NA
    bayes_gomp_a <- NA
      gomp.known_age(ind_df, known_age = "age",
                     silent.jags = TRUE,
                     silent.runjags = TRUE,
                     thinSteps = thinSteps,
                     numSavedSteps = numSavedSteps) %>%
        diagnostic.summary(., HDImass = 0.95) -> gomp_known_age_MCMC_diag
      bayes_gomp_b <- gomp_known_age_MCMC_diag[2,5]
      bayes_gomp_a <- gomp_known_age_MCMC_diag[1,5]
    
    # compute life table from "archaeological" data
      mort_prep_estim <- mortAAR::prep.life.table(ind_df, agebeg = "age_beg",
                                                  agerange = "exclude", method = c(1, 4, 5, 5, 3, 8, 10, 10, 75))
    mort_life_estim <- mortAAR::life.table(mort_prep_estim)
    x_length <- length(mort_life_estim$x)
    x_a <- cumsum(mort_life_estim$a)
    nax <- mort_life_estim$a[5:x_length]
    x_vec <- x_a[4:(x_length - 1)] - 15
    x_vec2 <- c(x_vec[-1], 115)
    x_mid <- ( x_vec + x_vec2 ) / 2
    Dx_vec <- mort_life_estim$Dx[5:x_length]
    qx_vec <- mort_life_estim$qx[5:x_length]
    lx_vec <- mort_life_estim$lx[5:x_length]/100
    mx_vec <- 1 / (nax * 100/qx_vec - nax/2)
    mort_df_estim <- data.frame(x_vec,  x_vec2, x_mid, Dx_vec, qx_vec, lx_vec, mx_vec)
    
    #fit WOLS to life table mx
    WOLS_estim_Gompertz_shape <- NA
    WOLS_estim_Gompertz_rate <- NA
    tryCatch({
      mort_fit_WOLS_estim <- lm(log(mx_vec)  ~ x_mid, data = mort_df_estim, weights  = Dx_vec)
      WOLS_estim_Gompertz_shape <- mort_fit_WOLS_estim$coefficients[2]
      WOLS_estim_Gompertz_rate <- exp(mort_fit_WOLS_estim$coefficients[1])
    }, error=function(e){})
    
    #fit OLS to life table mx
    OLS_estim_Gompertz_shape <- NA
    OLS_estim_Gompertz_rate <- NA
    tryCatch({
      mort_fit_OLS_estim <- lm(log(mx_vec)  ~ x_mid, data = mort_df_estim)
      OLS_estim_Gompertz_shape <- mort_fit_OLS_estim$coefficients[2]
      OLS_estim_Gompertz_rate <- exp(mort_fit_OLS_estim$coefficients[1])
    }, error=function(e){})
    
    # fit survival data with nls
    NLS_estim_Gompertz_shape <- NA
    NLS_estim_Gompertz_rate <- NA
    tryCatch({
      nls_estim_fit <- nls(lx_vec ~ exp(a/b - a/b * exp(b * x_vec ) ) , 
                           data = mort_df_estim, start=list(a = 0.001, b = 0.075), weights = Dx_vec)#, method="Nelder-Mead")
      NLS_estim_Gompertz_shape <- summary(nls_estim_fit)$coefficients[2]
      NLS_estim_Gompertz_rate <- summary(nls_estim_fit)$coefficients[1]
    }, error=function(e){})
    
    # #fit survival to estimated life table Dx
    surv_estim_lt_Gompertz_shape <- NA
    surv_estim_lt_Gompertz_rate <- NA
    mort_df_estim$death <- 1
    tryCatch({
      surv_estim_lt <- flexsurv::flexsurvreg(formula = survival::Surv(x_mid, death) ~ 1, data = mort_df_estim, dist="gompertz", weights = Dx_vec)
      surv_estim_lt_Gompertz_shape <- surv_estim_lt$coefficients[1]
      surv_estim_lt_Gompertz_rate <- exp(surv_estim_lt$coefficients[2])
    }, error=function(e){})
    
    # fit home-made MLE
    MLE_estim_lt_Gompertz_shape <- NA
    MLE_estim_lt_Gompertz_rate <- NA
    tryCatch({
      MLE_estim_lt <- Gomp.MLE.interval(mort_df_estim, agebegin = "x_vec", ageend = "x_vec2", Dx = "Dx_vec")
      MLE_estim_lt_Gompertz_shape <- MLE_estim_lt[2]
      MLE_estim_lt_Gompertz_rate <- MLE_estim_lt[1]
    }, error=function(e){})
    
    # fit bayesian poisson model to estimated data
    poisson.interval(mort_df_estim, age_beg = "x_vec", age_end = "x_vec2", Dx = "Dx_vec",
                       silent.jags = TRUE,
                       silent.runjags = TRUE,
                       thinSteps = 1,
                       numSavedSteps = 10000,
                       minimum_age = 0) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    bayes_estim_poisson_b <- gomp_anthr_MCMC_diag[2,5]
    bayes_estim_poisson_a <- gomp_anthr_MCMC_diag[1,5]
    
    #Bayesian model with anthropological age estimate
    bayes_anthr_gomp_b <- NA
    bayes_anthr_gomp_a <- NA
      gomp.anthr_age(ind_df, age_beg = "age_beg", age_end = "age_end",
                     silent.jags = TRUE,
                     silent.runjags = TRUE,
                     thinSteps = thinSteps,
                     numSavedSteps = numSavedSteps,
                     minimum_age = 15) %>%
        diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
      bayes_anthr_gomp_b <- gomp_anthr_MCMC_diag[2,5]
      bayes_anthr_gomp_a <- gomp_anthr_MCMC_diag[1,5]
    
    ind_result <- cbind(y = ind_df$n[1],
                        beta = ind_df$beta[1], alpha = ind_df$alpha[1],
                        surv_Gompertz_shape, surv_Gompertz_rate,
                        OLS_Gompertz_shape, OLS_Gompertz_rate,
                        WOLS_Gompertz_shape, WOLS_Gompertz_rate,
                        surv_lt_Gompertz_shape, surv_lt_Gompertz_rate,
                        bayes_anthr_gomp_5y_b, bayes_anthr_gomp_5y_a,
                        surv_lt10_Gompertz_shape, surv_lt10_Gompertz_rate,
                        bayes_anthr_gomp_10y_b, bayes_anthr_gomp_10y_a,
                        MLE_lt_Gompertz_shape, MLE_lt_Gompertz_rate,
                        MLE_lt10_Gompertz_shape, MLE_lt10_Gompertz_rate,
                        MLE_Gompertz_shape, MLE_Gompertz_rate,
                        MLE_adapted_Gompertz_shape, MLE_adapted_Gompertz_rate,
                        bayes_gomp_b, bayes_gomp_a,
                        WOLS_estim_Gompertz_shape, WOLS_estim_Gompertz_rate,
                        NLS_Gompertz_shape, NLS_Gompertz_rate,
                        OLS_estim_Gompertz_shape, OLS_estim_Gompertz_rate,
                        surv_estim_lt_Gompertz_shape, surv_estim_lt_Gompertz_rate,
                        NLS_estim_Gompertz_shape, NLS_estim_Gompertz_rate,
                        MLE_estim_lt_Gompertz_shape, MLE_estim_lt_Gompertz_rate,
                        bayes_anthr_gomp_b, bayes_anthr_gomp_a,
                        bayes_poisson_b, bayes_poisson_a,
                        bayes_estim_poisson_b, bayes_estim_poisson_a)
    lt_result <- rbind(lt_result, ind_result)
    
    svMisc::progress(g/sampling * 100, (sampling-1)/sampling * 100, progress.bar = TRUE)
    Sys.sleep(0.0001)
    if (g == sampling) message("Done!")
  }
  rownames(lt_result) <- NULL
  
  end_time <- Sys.time()
  print(end_time - start_time)
  return(lt_result)
}
