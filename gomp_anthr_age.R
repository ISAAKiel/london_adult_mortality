gomp.anthr_age <- function(x, # data.frame with needed columns
                           age_beg, # column name: documented age of the individual,
                           age_end,
                           minimum_age = 15,
                           numSavedSteps = numSavedSteps,
                           thinSteps = thinSteps,
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
  ones <- rep(1,Ntotal)
  C <- 100000
  
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
  # Be careful to specify only parameters which are actually used, otherwise you may confuse JAGS
  dataList = list(
    Ntotal = Ntotal ,
    C = C, # JAGS does not warn if too small!
    ones = ones,
    minimum_age = minimum_age,
    age_end = age_end,
    age_beg = age_beg,
    gomp_a0_m = gomp_a0[1],
    gomp_a0_ic = gomp_a0[2],
    gomp_a0_var = gomp_a0[3]
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
    for ( i in 1:Ntotal ) {
      age[i] ~ dunif(age_beg[i] - minimum_age, age_end[i] - minimum_age)
      age.s[i] <- age[i] + minimum_age
      spy[i] <- a * exp(b * age[i]) * exp(-a/b * (exp(b * age[i]) - 1)) / C # implementing Gompertz probability density
      ones[i] ~ dbern( spy[i]  )
      }
    b  ~ dgamma(0.01, 0.01) # a must not be null
    #log_a_M <- (-66.77 * (b - 0.0718) - 7.119) * (-1) # log_a_M must be positive to be used with dgamma
    #log_a  ~ dgamma(log_a_M^2 / 0.0823, log_a_M / 0.0823)
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
                  "M"  
                  #,"age.s"
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