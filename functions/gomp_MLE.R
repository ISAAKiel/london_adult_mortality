# from Konigsberg, http://faculty.las.illinois.edu/lylek/AHB2015/index.htm
Gomp.MLE <- function(x, age){
  t <- x[,age]
  N <- nrow(x)
  
  Gomp = function (x,age)
  {
    a3 = x[1]
    b3 = x[2]
    shift = 15
    h.t = a3*exp(b3*(age-shift))
    S.t = exp(a3/b3*(1-exp(b3*(age-shift))))
    return(log(S.t*h.t))
  }
  
  lnlk = function(x){
    zlnlk = 0
    for(i in 1:N) zlnlk = zlnlk + Gomp(x,t[i])
    return(-zlnlk)
  }
  
  hin=function(par){
    h=NA
    h[1] = par[1]
    h[2] = par[2]
    return(h)
  }
  
  start.theta = c(0.02,0.02)
  gomp_optim <-  alabama::constrOptim.nl(par=start.theta,fn=lnlk,hin=hin,control.outer=list(trace=F))
  return(gomp_optim$par)
}