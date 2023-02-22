# helper functions

# Approximate rate for hgompertz(x, shape, rate) 
# according Sasaki & Kondo 2016 fig. 1, line 6, 30: 
modal.age.b <- function (beta) {
  Sab <- -2.624
  Sbb <- 0.0393
  Ma <- -7.119
  Mb <- 0.0718
  M <- Sab * (beta - Mb) / Sbb + Ma
  return(M)
}

# after Pflaumer 2011, 734
gomp_lx <- function(x, a, b) {
  lx <- exp(a/b - a/b * exp(b * x))
  return(lx)
}

# derives from https://github.com/scpatricio/bell_mortality/blob/main/script/functions.R
# see also Castellares et al. 2020
# see also Frankenberg/Konigsberg 2006
gomp.ex <- function(x, a, b, age_start = 15) {
  t <- x - age_start
  
  E1 = function(z){
    integrate(function(t){
      (exp(-t))/t
    }, z, Inf)$value
  }
  
  ex <- exp(a * exp(b *t) / b) * E1(a * exp(b * t )/ b ) / b
  return(ex)
}

# function to calculate the realized fertility in relation to mortality
fertility_survival_mixture <- function(age_a, age_b, lower = 0, upper = 35) {
  
  # fertility model by Hassan 1981, 137 table 8.7
  fert <- data.frame(age = c(17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 50), fert = c(10.9, 23.1, 24.1, 19.5, 14.4, 6.2, 1.6, 0), fert_end = 1)
  
  surv_fert <- flexsurv::flexsurvreg(formula = survival::Surv(age - 15, fert_end) ~ 1, data = fert, dist="gompertz", weights = fert)
  fert_b <- surv_fert$coefficients[1]
  fert_a <- exp(surv_fert$coefficients[2])
  
  fert_f <- function(x, fert_a, fert_b, age_a, age_b) {
    # fertility Gompertz function
    fert_gomp <- fert_a * (exp(fert_b * x + fert_a/fert_b * (1- exp(fert_b * x))))
    
    # age survival
    age_surv <- exp(age_a / age_b * (1 - exp(age_b * x)))
    
    # multiplication of fertility and survival functions
    
    fert_gomp * age_surv
  }
  
  fert_sum <- integrate(fert_f, lower = lower, upper = upper, fert_a = fert_a, fert_b = fert_b, age_a = age_a, age_b = age_b)
  return(fert_sum$value)
}


beta_mom <- function(x) {
  
  m_x <- mean(x, na.rm = TRUE)
  s_x <- sd(x, na.rm = TRUE)
  
  alpha <- m_x*((m_x*(1 - m_x)/s_x^2) - 1)
  beta <- (1 - m_x)*((m_x*(1 - m_x)/s_x^2) - 1)
  
  return(list(alpha = alpha, beta = beta))
}

diagnostic.summary <- function(codaMCMClist, HDImass = 0.95, gelman_diag = TRUE) {
  parameterNames = varnames(codaMCMClist)
  mcmcMat = as.matrix(codaMCMClist,chains=TRUE)
  summaryInfo = NULL
  for ( parName in parameterNames ) {
    summaryInfo = rbind( summaryInfo , summarizePost( mcmcMat[,parName], credMass = HDImass ) )
    thisRowName = parName
    rownames(summaryInfo)[NROW(summaryInfo)] = thisRowName
  }
  summaryInfo_df <- as.data.frame(summaryInfo)
  if (gelman_diag == TRUE) {
    psrf_df <- as.data.frame((gelman.diag(codaMCMClist))$psrf)
    colnames(psrf_df) <- c("PSRF Point est.", "PSRF Upper C.I.")
    diagnostic_summary <- cbind(psrf_df, summaryInfo_df)
  } else {
    diagnostic_summary <- summaryInfo_df
    
  }
}

# simplified version of a similar function in Kruschke 2015
#' @rdname amtl_bayes_helper
#' @export
summarizePost = function( paramSampleVec , credMass=0.95 ) {
  meanParam = mean( paramSampleVec )
  medianParam = median( paramSampleVec )
  dres = density( paramSampleVec )
  modeParam = dres$x[which.max(dres$y)]
  mcmcEffSz = round( effectiveSize( paramSampleVec ) , 1 )
  names(mcmcEffSz) = NULL
  MCSE = sd(paramSampleVec)/sqrt(mcmcEffSz)
  hdiLim = HDIofMCMC( paramSampleVec , credMass=credMass )
  return( c( Mean=meanParam , Median=medianParam , Mode=modeParam ,
             ESS=mcmcEffSz , MCSE = MCSE,
             HDImass=credMass , HDIlow=hdiLim[1] , HDIhigh=hdiLim[2]) )
}

# simplified version of a similar function in Kruschke 2015
#' @rdname amtl_bayes_helper
#' @export
HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}


# simulation of populations
lt.sampling <- function(sampling,
                        n_min = 50,
                        n_max = 500,
                        b_min = 0.025,
                        b_max = 0.1) 
{  
  lt_result <- data.frame()
  for (g in 1:sampling) {
    y <- round(runif(n = 1, min = n_min, max = n_max))
    beta <- runif(n = 1, min = b_min, max = b_max)
    alpha <- exp(rnorm(1, (-66.77 * (beta - 0.0718) - 7.119), sqrt(0.0823) ) )
    ind_list <- data.frame(sampling_id = g, n = y, beta = beta, alpha = alpha, ind = 1:y) %>%
      mutate(age = round(flexsurv::rgompertz(n(), beta, alpha) ) + 15) %>% 
      mutate(age_beg = ifelse(age < 18, 15,
                              ifelse(age < 26, 18,
                                     ifelse(age < 36, 26,
                                            ifelse(age < 46, 36, 46))))) %>% 
      mutate(age_end = ifelse(age < 18, 18,
                              ifelse(age < 26, 26,
                                     ifelse(age < 36, 36,
                                            ifelse(age < 46, 46, 120)))))
    
    lt_result <- rbind(lt_result, ind_list)
  }
  return(lt_result)
}

# read WELLCOME database data: xlsx-files with two sheets
xls.amtl <- function(path) {
  options(dplyr.summarise.inform = FALSE)
  my_data1 <- readxl::read_excel(path, sheet = 1)
  my_data2 <- readxl::read_excel(path, sheet = 2)
  merged <- bind_rows(my_data1,my_data2)
  colnames(merged) <- c("site", "ind", "tp", "sex", "age", "tooth")
  merged_sub <- subset(merged, age > 5 & age < 12)
  ind_list <- merged_sub %>% group_by(ind, sex, age) %>% summarize(n())
  colnames(ind_list) <-c("ind", "sex", "age", "tp")
  ind_list <- ind_list[,-4]
  return(ind_list)
}


# this function generates starting values for the Gompertz distribution
# if the starting age is not 15
gomp.a0 <- function(
    sampling = 100000,
    b_min = 0.02,
    b_max = 0.1,
    minimum_age = 15) {
  
  # we do not want too much overhead so no computation if the default age of 15 is true
  if (minimum_age == 15) {
    fit_coeff <- c(-66.77, -2.324914, 0.0823) 
  } else {
    null_age <- minimum_age - 15
    
    ind_df <- data.frame(b = runif(n = sampling, min = b_min, max = b_max)) %>%
      mutate(a = exp(rnorm(n(), (-66.77 * (b - 0.0718) - 7.119), sqrt(0.0823) ))) %>% 
      mutate(a0 = a * exp(b * null_age))
    
    fit <- lm(log(a0) ~ b, data = ind_df)
    rse <- sum(fit$residuals**2)/fit$df.residual # without squaring
    fit_coeff <- c(fit$coefficients[2], fit$coefficients[1], rse )
    fit_coeff <- unname(fit_coeff)
  }
  return(fit_coeff)
}