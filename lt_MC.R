lt.MC <- function(sampling,
                  n_min = 50,
                  n_max = 500,
                  M_min = NULL,
                  M_max = NULL,
                  b_min = 0.025,
                  b_max = 0.1,
                  error_range = NULL,
                  age_categories = "Wellcome", # either Museum of London Wellcome database or of the Backbone of Europe project
                  bayes = FALSE,
                  thinSteps = 10,
                  numSavedSteps = 10000 )
{
  start_time <- Sys.time()
  lt_result <- data.frame()
  for (g in 1:sampling) {
    # sampling with Gompertz distribution
    y <- round(runif(n = 1, min = n_min, max = n_max))
    if(is.null(M_min) | is.null(M_max)) {
      b_ <- runif(n = 1, min = b_min, max = b_max)
      a_ <- exp(rnorm(1, (-66.77 * (b_ - 0.0718) - 7.119), sqrt(0.0823) ) )
    } else {
      M <- round(runif(n = 1, min = M_min, max = M_max))
      M_1 <- 0
      M_2 <- 0
      while ( (M < M_1 | M > M_2 )) {
        b_ <- runif(n = 1, min = b_min, max = b_max)
        a_ <- exp(rnorm(1, (-66.77 * (b_ - 0.0718) - 7.119), sqrt(0.0823) ) )
        M_ <- 1 / b_ * log (b_/a_) + 15
        M_1 <- M_ - 1
        M_2 <- M_ + 1
      }
    }

    ind_df <- data.frame(ind = NA, age = NA, age_beg = NA, age_end = NA)
    for (i in 1:y) {
      x <- round(flexsurv::rgompertz(1, b_, a_) ) + 15
      if(length(error_range) > 0) {
        x_used <- round(rnorm(1, x, error_range))
      } else {
        x_used <- x
      }
      if(age_categories == "Wellcome") {
        if(x_used < 18) {
          age_beg = 12
          age_end = 17
        } else if(x_used < 26) {
          age_beg = 18
          age_end = 25
        } else if(x_used < 36) {
          age_beg = 26
          age_end = 35
        } else if(x_used < 46) {
          age_beg = 36
          age_end = 45
        } else {
          age_beg = 46
          age_end = 99
        } 
      } else {
        if(x_used < 20) {
          age_beg = 15
          age_end = 19
        } else if(x_used < 25) {
          age_beg = 20
          age_end = 24
        } else if(x_used < 30) {
          age_beg = 25
          age_end = 29
        } else if(x_used < 35) {
          age_beg = 30
          age_end = 34
        } else if(x_used < 40) {
          age_beg = 35
          age_end = 39
        } else {
          age_beg = 40
          age_end = 99
        } 
      }
      
      ind_df <- rbind(ind_df, c(i, x, age_beg, age_end))
    }
    ind_df <- ind_df[-1,]
    
    # fit Gompertz distribution to known age with Survival package + individual age
    ind_df$death <- 1
    ind_dfGomp <- flexsurv::flexsurvreg(formula = survival::Surv(age - 15, death) ~ 1, data = ind_df, dist="gompertz")
    surv_Gompertz_shape <- ind_dfGomp$coefficients[1]
    surv_Gompertz_rate <- exp(ind_dfGomp$coefficients[2])
    
    # compute life table
    ind_df$age_1 <- ind_df$age + 1
    mort_prep_x <- mortAAR::prep.life.table(ind_df, agebeg = "age", ageend = "age_1", agerange = "excluded")
    mort_life_x <- mortAAR::life.table(mort_prep_x)
    x_length <- length(mort_life_x$x)
    x_a <- cumsum(mort_life_x$a)
    x_vec <- x_a[4:(x_length - 1)] - 15
    Dx_vec <- mort_life_x$Dx[5:x_length]
    qx_vec <- mort_life_x$qx[5:x_length]/100
    lx_vec <- mort_life_x$lx[5:x_length]/100
    Lx_vec <- mort_life_x$Lx[5:x_length]
    dx_vec <- mort_life_x$dx[5:x_length]
    mx_vec <- dx_vec / Lx_vec
    kx <- Dx_vec / ( mx_vec * 5 )
    mort_df_x <- data.frame(x_vec, Dx_vec, qx_vec, lx_vec, mx_vec, kx)
    
    # #fit OLS to life table qx which has to by divided by 1,000
    OLS_Gompertz_shape <- NA
    OLS_Gompertz_rate <- NA
    tryCatch({
      mort_fit_OLS <- lm(log(mx_vec)  ~ x_vec, data = mort_df_x)
      OLS_Gompertz_shape <- mort_fit_OLS$coefficients[2]
      OLS_Gompertz_rate <- exp(mort_fit_OLS$coefficients[1])
    }, error=function(e){})
    
    #fit WOLS to life table qx which has to by divided by 1,000
    mort_fit_WOLS <- lm(log(mx_vec)  ~ x_vec, data = mort_df_x, weights  = Dx_vec) # experimenting with midpoints
    WOLS_Gompertz_shape <- mort_fit_WOLS$coefficients[2]
    WOLS_Gompertz_rate <- exp(mort_fit_WOLS$coefficients[1])
    
    # fit survival data with nls via lx
    NLS_Gompertz_shape <- NA
    NLS_Gompertz_rate <- NA
    tryCatch({
      nls_fit <- nls(lx_vec ~ exp(a/b - a/b * exp(b * x_vec ) ) , 
                     data = mort_df_x, start=list(a = 0.001, b = 0.075), weights = Dx_vec)#, method="Nelder-Mead")
      NLS_Gompertz_shape <- summary(nls_fit)$coefficients[2]
      NLS_Gompertz_rate <- summary(nls_fit)$coefficients[1]
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
    
    # create life table with broader age ranges
    mort_prep_x <- mortAAR::prep.life.table(ind_df, 
       agebeg = "age", ageend = "age_1", agerange = "exclude", 
       method = c(1, 4, 5, 5, 5, 10, 10, 10, 10, 10, 10, 10, 10))
    mort_life_x <- mortAAR::life.table(mort_prep_x)
    x_length <- length(mort_life_x$x)
    x_a <- cumsum(mort_life_x$a)
    x_vec <- x_a[4:(x_length - 1)] - 15
    Dx_vec <- mort_life_x$Dx[5:x_length]
    mort_df_x <- data.frame(x_vec, Dx_vec)
    
    # #fit survival to life table Dx
    surv_lt10_Gompertz_shape <- NA
    surv_lt10_Gompertz_rate <- NA
    mort_df_x$death <- 1
    tryCatch({
      surv_lt10 <- flexsurv::flexsurvreg(formula = survival::Surv(x_vec, death) ~ 1, data = mort_df_x, dist="gompertz", weights = Dx_vec)
      surv_lt10_Gompertz_shape <- surv_lt10$coefficients[1]
      surv_lt10_Gompertz_rate <- exp(surv_lt10$coefficients[2])
    }, error=function(e){})
    
    #   # fit survival data with nls via qx
    #   NLS_Gompertz_shape_qx <- NA
    #   NLS_Gompertz_rate_qx <- NA
    #   tryCatch({
    #     nls_fit_qx <- nls(qx_vec ~ a * exp(b * x_vec ) , 
    #                       data = mort_df_x, start=list(a = 0.001, b = 0.05), weights = Dx_vec)#, method="Nelder-Mead")
    #     NLS_Gompertz_shape_qx <- summary(nls_fit_qx)$coefficients[2]
    #     NLS_Gompertz_rate_qx <- summary(nls_fit_qx)$coefficients[1]
    # }, error=function(e){})
    
    # #fit Loss functions of package MortalityLaws
    # mort_life_law_LF2 <- MortalityLaws::MortalityLaw(x_mid, qx = 0.001 * qx_vec, law = 'gompertz', opt.method = 'LF2')
    # LF2_Gompertz_shape <- mort_life_law_LF2$coefficients[2]
    # LF2_Gompertz_rate <- mort_life_law_LF2$coefficients[1]
    # 
    # mort_life_law_poisson <- MortalityLaws::MortalityLaw(x_mid, qx = 0.001 * qx_vec, law = 'gompertz', opt.method = 'poissonL')
    # poisson_Gompertz_shape <- mort_life_law_poisson$coefficients[2]
    # poisson_Gompertz_rate <- mort_life_law_poisson$coefficients[1]
    # 
    # mort_life_law_binom <- MortalityLaws::MortalityLaw(x_mid, qx = 0.001 * qx_vec, law = 'gompertz', opt.method = 'binomialL')
    # binom_Gompertz_shape <- mort_life_law_binom$coefficients[2]
    # binom_Gompertz_rate <- mort_life_law_binom$coefficients[1]
    
    # Bayes model
    bayes_gomp_b <- NA
    bayes_gomp_a <- NA
    if (bayes == TRUE) {
      gomp.known_age(ind_df, known_age = "age",
                     silent.jags = TRUE,
                     silent.runjags = TRUE,
                     thinSteps = thinSteps,
                     numSavedSteps = numSavedSteps) %>%
        diagnostic.summary(., HDImass = 0.95) -> gomp_known_age_MCMC_diag
      bayes_gomp_b <- gomp_known_age_MCMC_diag[2,5]
      bayes_gomp_a <- gomp_known_age_MCMC_diag[1,5]
    }
    
    
    # compute life table from "archaeological" data
    if(age_categories == "Wellcome") {
      mort_prep_estim <- mortAAR::prep.life.table(ind_df, agebeg = "age_beg",
                                                  ageend = "age_end", agerange = "include", method = c(1, 4, 5, 2, 6, 8, 10, 10, 54))
    } else {
      mort_prep_estim <- mortAAR::prep.life.table(ind_df, agebeg = "age_beg",
                                                  ageend = "age_end", agerange = "include", method = c(1, 4, 5, 5, 5, 5, 5, 5, 5, 59))
    }
    mort_life_estim <- mortAAR::life.table(mort_prep_estim)
    x_length <- length(mort_life_estim$x)
    x_a <- cumsum(mort_life_estim$a)
    nax <- mort_life_estim$a[5:x_length]
    x_vec <- x_a[4:(x_length - 1)] - 15
    #x_vec <- x_a[4:(x_length - 1)] - 12
    x_vec2 <- c(x_vec[-1], Inf)
    x_mid <- ( x_vec + c(x_vec[-1], 99) ) / 2
    Dx_vec <- mort_life_estim$Dx[5:x_length]
    qx_vec <- mort_life_estim$qx[5:x_length]
    lx_vec <- mort_life_estim$lx[5:x_length]/100
    mx_vec <- 1 / (nax * 100/qx_vec - nax/2)
    mort_df_estim <- data.frame(x_vec,  x_vec2, x_mid, Dx_vec, qx_vec, lx_vec, mx_vec)
    
    #fit WOLS to life table qx which has to by divided by 1,000
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
    surv_estim_lt_Gompertz_shape <- NULL
    surv_estim_lt_Gompertz_rate <- NULL
    mort_df_estim$death <- 1
    tryCatch({
      surv_estim_lt <- flexsurv::flexsurvreg(formula = survival::Surv(x_mid, death) ~ 1, data = mort_df_estim, dist="gompertz", weights = Dx_vec)
      surv_estim_lt_Gompertz_shape <- surv_estim_lt$coefficients[1]
      surv_estim_lt_Gompertz_rate <- exp(surv_estim_lt$coefficients[2])
    }, error=function(e){})
    
    # # fit Gompertz distribution to age estimations with Survival package
    # ind_df_estim_Gomp <- flexsurv::flexsurvreg(formula = survival::Surv(age_beg, age_end, 
    #   type = "interval2") ~ 1, data = ind_df, dist="gompertz", method="Nelder-Mead") # default method throws error
    # surv_ind_estim_Gompertz_shape <- ind_df_estim_Gomp$coefficients[1]
    # surv_ind_estim_Gompertz_rate <- exp(ind_df_estim_Gomp$coefficients[2])
    
    #Bayesian modell with anthropological age estimate
    bayes_anthr_gomp_b <- NA
    bayes_anthr_gomp_a <- NA
    if (bayes == TRUE) {
      gomp.anthr_age(ind_df, age_beg = "age_beg", age_end = "age_end",
                     silent.jags = TRUE,
                     silent.runjags = TRUE,
                     thinSteps = thinSteps,
                     numSavedSteps = numSavedSteps) %>%
        diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
      bayes_anthr_gomp_b <- gomp_anthr_MCMC_diag[2,5]
      bayes_anthr_gomp_a <- gomp_anthr_MCMC_diag[1,5]
    }
    
    ind_result <- cbind(y, #M, 
                        a_, b_, surv_Gompertz_shape, surv_Gompertz_rate,
                        OLS_Gompertz_shape, OLS_Gompertz_rate,
                        WOLS_Gompertz_shape, WOLS_Gompertz_rate,
                        surv_lt_Gompertz_shape, surv_lt_Gompertz_rate,
                        surv_lt10_Gompertz_shape, surv_lt10_Gompertz_rate,
                        #LF2_Gompertz_shape, LF2_Gompertz_rate,
                        #poisson_Gompertz_shape, poisson_Gompertz_rate,
                        #binom_Gompertz_shape, binom_Gompertz_rate,
                        WOLS_estim_Gompertz_shape, WOLS_estim_Gompertz_rate,
                        OLS_estim_Gompertz_shape, OLS_estim_Gompertz_rate,
                        surv_estim_lt_Gompertz_shape, surv_estim_lt_Gompertz_rate,
                        #surv_ind_estim_Gompertz_shape, surv_ind_estim_Gompertz_rate,
                        NLS_Gompertz_shape, NLS_Gompertz_rate,
                        #NLS_Gompertz_shape_qx, NLS_Gompertz_rate_qx,
                        NLS_estim_Gompertz_shape, NLS_estim_Gompertz_rate,
                        bayes_gomp_b, bayes_gomp_a,
                        bayes_anthr_gomp_b, bayes_anthr_gomp_a)
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
