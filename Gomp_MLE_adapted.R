# adapted and simplified from Konigsberg, http://faculty.las.illinois.edu/lylek/AHB2015/index.htm
Gomp.MLE.adapted <- function(x, age){
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
    for(i in 1:N) {
      zlnlk = zlnlk + Gomp(x,t[i])
    }
    return(zlnlk)
  }
  
  gomp_optim <- optim(c(0.01, 0.01), lnlk, control = list(fnscale=-1))
  return(gomp_optim$par)
}
