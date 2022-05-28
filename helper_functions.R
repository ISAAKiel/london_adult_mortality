# helper functions

gomp_lx <- function(x, a, b) {
  lx <- exp(a/b - a/b * exp(b * x))
  return(lx)
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
                        b_max = 0.1,
                        error_range = NULL) 
{
  start_time <- Sys.time()
  lt_result <- data.frame()
  for (g in 1:sampling) {
    y <- round(runif(n = 1, min = n_min, max = n_max))
    b_ <- runif(n = 1, min = b_min, max = b_max)
    a_ <- exp(rnorm(1, (-66.77 * (b_ - 0.0718) - 7.119), sqrt(0.0823) ) )
    ind_list <- data.frame()
    for (i in 1:y) {
      x <- round(flexsurv::rgompertz(1, b_, a_) ) + 15
      if(length(error_range) > 0) {
        x_used <- round(rnorm(1, x, error_range))
      } else {
        x_used <- x
      }      
      if (x_used > 99) {
        x_used = 99
      } else if 
      (x_used < 15) {
        x_used = 15
      }
      x_diff <- x - x_used
      x_abs <- abs(x - x_used)
      ind_x <- cbind(i, x_diff, x_abs)
      ind_list <- rbind(ind_list, ind_x)
    }
    lt_bias <- mean(ind_list$x_diff)
    lt_bias_sd <- sd(ind_list$x_diff)
    lt_inaccuracy <- mean(ind_list$x_abs)
    ind_result <- cbind(y, b_, a_, error_range, lt_bias, lt_bias_sd, lt_inaccuracy)
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