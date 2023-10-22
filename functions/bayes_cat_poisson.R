poisson.interval <- function(x, # data.frame with needed columns
                             Dx,#number of deaths
                             age_beg,
                             age_end,
                             minimum_age = 15,
                             numSavedSteps = 10000,
                             thinSteps=1,
                             runjagsMethod="rjags",
                             nChains=3,
                             adaptSteps = 1000,
                             burnInSteps = 2000,
                             silent.jags = FALSE,
                             silent.runjags = FALSE) {
  
  require(coda)
  require(runjags)
  
  age_beg = x[,age_beg]
  age_end = x[,age_end]
  Ntotal <- length(age_beg) # number of individuals
  Dx <- round(x[,Dx])
  Dx_total <- sum(Dx)
  
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
  
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    Ntotal = Ntotal,
    minimum_age = minimum_age,
    Dx = Dx,
    Dx_total = Dx_total,
    age_beg = age_beg,
    age_end = age_end,
    gomp_a0_m = gomp_a0[1],
    gomp_a0_ic = gomp_a0[2],
    gomp_a0_var = gomp_a0[3]
  )
  
  # THE MODEL.
  modelString = "
  model {
    for ( j in 1:Ntotal ) {
      age[j] ~ dunif(age_beg[j] - minimum_age, age_end[j] - minimum_age)
      age.s[j] <- age[j] + minimum_age
      Dx[j] ~ dpois(lambda[j])
      lambda[j] <-  mx[j] * K[j]
      K[j] <- Dx_total * exp(-a/b * (exp(b * age[j]) - 1))
      mx[j] <- a * exp(b * age[j])
    }  
    b  ~ dgamma(0.01, 0.01) T(0.02,) # a must not be null
    log_a_M <- (gomp_a0_m * b + gomp_a0_ic) * (-1) # log_a_M must be positive to be used with dgamma
    log_a  ~ dgamma(log_a_M^2 / gomp_a0_var, log_a_M / gomp_a0_var)
    a <- exp(log_a * (-1))
    M <- 1 / b * log (b/a) + minimum_age
  }
  " # close quote for modelString
  
  runjags.options(silent.jags = silent.jags,
                  silent.runjags = silent.runjags)
  
  # RUN THE CHAINS
  parameters = c( "a", "b", "M")
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