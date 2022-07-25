# function for estimating Gompertz parameters via Maximum likelihood estimation,
# derived from Batey 2012, 161

Gomp.MLE.interval <- function(x, agebegin, ageend, Dx) {
  age_beg = x[,agebegin]
  age_end = x[,ageend]
  Dx = x[,Dx]
  death_df <- data.frame(age_beg, age_end, Dx)
  death_df <- subset(death_df, Dx > 0)
  nrow <- nrow(death_df)
  
  Gomp.intern <- function(x, deaths = death_df){
    alpha = x[1]
    beta = x[2]
    #shift<- 15 # only ages 15 and up are considered
    nrow <- NROW(deaths)
    S.t<- function(t){
      return(exp(alpha/beta*(1-exp(beta * t ) ) ) )
    }
    d <- S.t(deaths[1:nrow,1])-S.t(deaths[1:nrow,2])
    obs <- deaths[,3]
    lnlk <- as.numeric(crossprod(obs,log(d)))
    
    return(lnlk)
  }
  gomp_optim <- optim(c(0.01, 0.01), Gomp.intern, control = list(fnscale=-1))
  return(gomp_optim$par)
}
