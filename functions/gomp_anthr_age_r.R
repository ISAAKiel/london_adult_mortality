gomp.anthr_age.r <- function(x, # data.frame with needed columns
                           age_beg, 
                           age_end,
                           minimum_age = 15,
                           maximum_age = 120,
                           numSavedSteps = numSavedSteps,
                           thinSteps = thinSteps,
                           runjagsMethod="rjags",
                           nChains=3,
                           adaptSteps = 1000,
                           burnInSteps = 2000,
                           silent.jags = FALSE,
                           silent.runjags = FALSE,
                           r = 0) {
  
  require(coda)
  require(runjags)
  
  age_beg <- x[,age_beg]
  age_end <- x[,age_end]
  Ntotal <- length(age_beg) # number of individuals  
  ones <- rep(1,Ntotal)
  age_abs <- seq(0, (maximum_age - minimum_age), 1) # for integral calculation in 1-year-intervals
  
  # Generate values for Gompertz alpha if minimum age is not 15
  gomp_a0 <- gomp.a0(minimum_age = minimum_age)
  
  # Generate inits-list with custom function,
  # including RNGs and seed for JAGS for reproducible results
  initsList <- function(){
    RNG_list <- c("base::Wichmann-Hill",
                  "base::Marsaglia-Multicarry",
                  "base::Super-Duper",
                  "base::Mersenne-Twister")
    init_list <- list(
      .RNG.name = sample(RNG_list, 1),
      .RNG.seed = sample(1:1e+06, 1),
      b = rnorm(1, 0.05, 0.005)
    )
    return(init_list)
  }
  
  dataList = list(
    Ntotal = Ntotal,
    ones = ones,
    minimum_age = minimum_age,
    maximum_age = maximum_age,
    age_end = age_end,
    age_beg = age_beg,
    gomp_a0_m = gomp_a0[1],
    gomp_a0_ic = gomp_a0[2],
    gomp_a0_var = gomp_a0[3],
    r = r,
    age_abs = age_abs
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
    for ( i in 1:Ntotal ) {
      age[i] ~ dunif(age_beg[i] - minimum_age, age_end[i] - minimum_age)
      age.s[i] <- age[i] + minimum_age
      spy[i] <- rate_exp[i] * a * exp(b * age[i]) * exp(-a/b * (exp(b * age[i]) - 1)) / unity
      rate_exp[i] <- exp(-r_ * age[i] )
      ones[i] ~ dbern( spy[i]  )
    }
  unity <- sum(gomp_sum[1:(1 + maximum_age - minimum_age)])
  for (j in 1:(1 + maximum_age - minimum_age)) {
      gomp_sum[j] <- exp(-r_ * (age_abs[j])) * a * exp(b * age_abs[j]) * exp(-a/b * (exp(b * age_abs[j]) - 1))

  }
    r_  ~ dnorm(r, 1/0.0025^2) 
    b  ~ dgamma(0.01, 0.01) # a must not be null
    log_a_M <- (gomp_a0_m * b + gomp_a0_ic) * (-1) # log_a_M must be positive to be used with dgamma
    log_a  ~ dgamma(log_a_M^2 / gomp_a0_var, log_a_M / gomp_a0_var)
    a <- exp(log_a * (-1))
    M <- 1 / b * log (b/a) + minimum_age
  }
  " # close quote for modelString
  
  runjags.options(silent.jags = silent.jags,
                  silent.runjags = silent.runjags)
  
  # RUN THE CHAINS
  parameters = c( "a", "b", 
                  "M" , "unity", "r_"
  )
  runJagsOut <- run.jags( method = runjagsMethod ,
                          model = modelString ,
                          monitor = parameters ,
                          data = dataList ,
                          inits = initsList ,
                          n.chains = nChains ,
                          adapt = adaptSteps ,
                          burnin = burnInSteps ,
                          sample = ceiling(numSavedSteps/nChains) ,
                          thin = thinSteps ,
                          summarise = TRUE ,
                          plots = TRUE )
  codaSamples = as.mcmc.list( runJagsOut )
  return(codaSamples)
}