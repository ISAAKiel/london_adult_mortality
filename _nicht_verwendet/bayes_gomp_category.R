# St. Bride's crypt
molas_data <- c("../Wellcome_Database/ST\ BRIDES\ CRYPT_SB79_DENTAL\ DATA_NILS\ MUELLER-SCHEESSEL_SEPT\ 2021.xlsx")
my_data3 <- readxl::read_excel(molas_data, sheet = 3) [, 1:8]
colnames(my_data3) <- c("site", "ind", "birth", "death", "known_sex", "known_age", "sex", "age")
stbrides <- as.data.frame(my_data3)
stbrides$known_age <- as.integer(stbrides$known_age)
stbrides$birth <- as.integer(stbrides$birth)
stbrides$death <- as.integer(stbrides$death)
stbrides <- na.omit(stbrides)
stbrides <- subset(stbrides, known_age >= 12 & ind != 105)

length_stbrides <- nrow(stbrides)
for (i in 1:length_stbrides) {
  if(stbrides$age[i] == 6) {
    stbrides$age_beg[i] <-  12
    stbrides$age_end[i] <-  18
  } else if(stbrides$age[i] == 7) {
    stbrides$age_beg[i] <-  18
    stbrides$age_end[i] <-  26
  } else if(stbrides$age[i] == 8) {
    stbrides$age_beg[i] <-  26
    stbrides$age_end[i] <-  36
  } else if(stbrides$age[i] == 9) {
    stbrides$age_beg[i] <-  36
    stbrides$age_end[i] <-  46
  } else if(stbrides$age[i] == 10) {
    stbrides$age_beg[i] <-  46
    stbrides$age_end[i] <-  100
  } else if(stbrides$age[i] == 11) {
    stbrides$age_beg[i] <-  18
    stbrides$age_end[i] <-  100
  } 
}

gomp.anthr_age.cat(stbrides, age_beg = "age_beg", age_end = "age_end", category = "sex",
                   thinSteps = 10, minimum_age = 12,
                   numSavedSteps = 100000) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag


gomp.anthr_age.cat <- function(x, # data.frame with needed columns
                           age_beg, # column name: documented age of the individual,
                           age_end,
                           category,
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
  c = as.numeric(as.factor(x[,category]))
  Ncat = length(unique(c))
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
      .RNG.seed = sample(1:1e+06, 1)
      #,
      #b = rnorm(1, 0.05, 0.005)
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
    c = c,
    Ncat = Ncat,
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
      spy[i] <- a[c[i]] * exp(b[c[i]] * age[i]) * exp(-a[c[i]]/b[c[i]] * (exp(b[c[i]] * age[i]) - 1)) / C # implementing Gompertz probability density
      ones[i] ~ dbern( spy[i]  )
    }
        for ( k in 1:Ncat ) {
          b[k] ~ dgamma(beta_gamma_mu, 1 / beta_gamma_sd^2)
          log_a_M[k] <- (gomp_a0_m * b[k] + gomp_a0_ic) * (-1) # log_a_M must be positive to be used with dgamma
          log_a[k]  ~ dgamma(log_a_M[k]^2 / gomp_a0_var, log_a_M[k] / gomp_a0_var)
          a[k] <- exp(log_a[k] * (-1))
          M[k] <- 1 / b[k] * log (b[k]/a[k]) + minimum_age
        }
    beta_gamma_mu ~ dgamma(0.01, 0.01)
    beta_gamma_sd ~ dgamma(0.01, 0.01)

  }
  " # close quote for modelString
  
  runjags.options(silent.jags = silent.jags,
                  silent.runjags = silent.runjags)
  
  # RUN THE CHAINS
  parameters = c( "beta_gamma_mu", "beta_gamma_sd", "a", "b", 
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
