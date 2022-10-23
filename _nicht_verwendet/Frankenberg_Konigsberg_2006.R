# Gompertz
Gompertz <- function (x) {
  a <- x[1] # put x vector in a & b
  b <- x[2]
  t <- c(0, 100/3, 200/3, 100) # ages are O, 33.33, 66.67 & 100
  l <- exp(a/b*(1 - exp(b*t) )) # Gompertz survivorship at ages
  d <- l[1:3]-l[2:4]
  obs <- c(.5, .4, .1) # difference l(t) to get d(x) # observed d(x) for Pecos Pueblo
  lnlk <- obs %*% log(d)  # form the log-likelihood
  return(lnlk) # return log-likelihood
}
optim(c(0.001, 0.001), Gompertz, control=list (fnscale=-1) )

# Siler
age1 <- c(0, 1, 3, 10, 20, 30, 50, 20)
age2 <- c(1, 3, 10, 20, 30, 50, 80, 80)
pecos_count <- c(322, 117, 120, 145, 108, 772, 189, 51)
pecos <- data.frame(age1, age2, pecos_count)

Gompertz <- function(x, deaths = pecos) { 
  a1 <- x[1]
  b1 <- x[2]
  a3 <- x[3]
  b3 <- x[4]
  t<-deaths[,1:2]
  l <- exp(-a1/b1*(1-exp(-b1*t)))*exp(a3/b3*(1- exp(b3*t)))
  d<-l[,1]- l[,2]
  obs<-deaths[,3]
  lnlk<-crossprod(obs,log(d)) 
  return(lnlk)
}
optim(c(0.2, 0.7, 0.002, 0.08), Gompertz, control=list (fnscale=-1) )

# population decrease

Gompertz <- function(x) { 
  a1 <- x[1]
  b1<-  x[2]
  a3 <- x[3]
  b3 <- x[4]
  t<-pecos[1:7,1]
  r = -.015
  # ipdf <- function(t) {
  #   dgpmp <- exp(-a1/b1*(1-exp(-b1*t)))*exp(a3/b3*(1- exp(b3*t)))*exp(-r*t)*(a1*exp(-b1*t)+a3*exp(b3*t))
  #   return(dgomp)
  #   }
  # for(i in 1:7) {
  #   L[i]<-integrate(ipdf,t[i],80)$value
  # }
  integrand <- function(t) {
    exp(-a1/b1*(1-exp(-b1*t)))*exp(a3/b3*(1- exp(b3*t)))*exp(-r*t)*(a1*exp(-b1*t)+a3*exp(b3*t))
  }
  L <- NULL
  for(i in 1:7) {
    L[i]<-integrate(integrand,lower = t[i], upper = 80, abs.tol=1E-7)$value
  }
  
  L<-L/L[1]
  d<-L[1:6]-L[2:7]
  d<-c(d,L[7],1-d[1]-d[2]-d[3])
  obs<-pecos[,3]
  lnlk <- crossprod(obs,log(d))
  return(lnlk)
}
optim(c(0.2, 0.7, 0.002, 0.08), Gompertz, control=list (fnscale=-1) , method = "Nelder-Mead")

year_data_uncount <- pecos[-c(1:4),] %>% uncount(pecos_count)
gomp.anthr_age.r(year_data_uncount, age_beg = "age1", age_end = "age2",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 1,
                 numSavedSteps = 10000,
                 minimum_age = 20) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
