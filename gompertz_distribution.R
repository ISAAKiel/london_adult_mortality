gridExtra::grid.arrange (
  
  ggplot() + xlim(15, 100) + ylim(0, 0.4) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.025, exp((-66.77) * (0.025 - 0.0718) - 7.119)), 
                  aes(col = "\u03B2 = 0.025")) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.04,  exp((-66.77) * (0.04 - 0.0718) - 7.119)),
                  aes(col = "\u03B2 = 0.04")) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.06,  exp((-66.77) * (0.06 - 0.0718) - 7.119)), 
                  aes(col = "\u03B2 = 0.06")) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.09,  exp((-66.77) * (0.09 - 0.0718) - 7.119)), 
                  aes(col = "\u03B2 = 0.9")) +
    ylab("hazard") + xlab("age in years") + 
    scale_colour_manual(values = c("red","blue","green", "dark grey")) +
    theme(legend.position = c(0.2, 0.7), legend.title = element_blank()),
  
  ggplot() + xlim(15, 105) +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.025, exp((-66.77) * (0.025 - 0.0718) - 7.119))), colour = "red") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.04,  exp((-66.77) * (0.04 - 0.0718) - 7.119))), colour= "blue") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.06,  exp((-66.77) * (0.06 - 0.0718) - 7.119))), colour= "green") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.09,  exp((-66.77) * (0.09 - 0.0718) - 7.119))), colour= "dark grey") +
    xlab("age in years") + ylab("hazard (log scale)"),
  
  ggplot() + xlim(15, 105) +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.025, exp((-66.77) * (0.025 - 0.0718) - 7.119)), colour = "red") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.04,  exp((-66.77) * (0.04 - 0.0718) - 7.119)), colour= "blue") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.06,  exp((-66.77) * (0.06 - 0.0718) - 7.119)), colour= "green") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.09,  exp((-66.77) * (0.09 - 0.0718) - 7.119)), colour= "dark grey") +
    xlab("age in years")  + ylab("density"),
  
  ggplot() + xlim(15, 105) + ylim(0, 1) +
    geom_function(fun = function(x) gomp_lx(x - 15, exp((-66.77) * (0.025 - 0.0718) - 7.119), 0.025), colour = "red") +
    geom_function(fun = function(x) gomp_lx(x - 15,  exp((-66.77) * (0.04 - 0.0718) - 7.119), 0.04), colour = "blue") +
    geom_function(fun = function(x) gomp_lx(x - 15,  exp((-66.77) * (0.06 - 0.0718) - 7.119), 0.06), colour = "green") +
    geom_function(fun = function(x) gomp_lx(x - 15,  exp((-66.77) * (0.09 - 0.0718) - 7.119), 0.09), colour = "dark grey") +
    ylab("survival") + xlab("age in years"),
  
  ncol = 2
)
