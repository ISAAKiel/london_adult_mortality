#code from L. Konigsberg: http://faculty.las.illinois.edu/lylek/Demog/Demography.html
Gomp.MLE.interval <- function(x, agebegin, ageend, Dx) {
  age_beg = x[,agebegin]
  age_end = x[,ageend]
  Dx = x[,Dx]
  death_df <- data.frame(age_beg, age_end, Dx)
  death_df <- subset(death_df, Dx > 0)
  nrow <- nrow(death_df)
  
  S=function(x,t){
    a=x[1]
    b=x[2]
    return(exp(a/b*(1-exp(b*t))))
  }
  x=c(0.05, 0.02)
  
  lnLK=function(x){
    zloglik=0
    for(i in 1:(nrow - 1) ) {
      zloglik = zloglik + death_df[i,3] * log(S(x, death_df[i,1]) - S(x, death_df[i,2]))
    }
    zloglik = zloglik + death_df[nrow, 3] * log(S(x, death_df[nrow, 1]))
    return(as.numeric(zloglik))
  }
  
  hin = function(x) {
    h = vector()
    h[1] = x[1]
    h[2] = x[2]
    return(h)
  }
  gomp_optim <- alabama::constrOptim.nl(par = c(0.05, 0.02), lnLK, hin = hin, control.optim = list(fnscale=-1))
  return(gomp_optim$par)
}
