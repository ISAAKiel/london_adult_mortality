gridExtra::grid.arrange (
  
  ggplot() + xlim(15, 100) + ylim(0, 0.4) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.025, exp((-66.77) * (0.025 - 0.0718) - 7.119)), colour = "red") +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.04,  exp((-66.77) * (0.04 - 0.0718) - 7.119)), colour= "blue") +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.06,  exp((-66.77) * (0.06 - 0.0718) - 7.119)), colour= "green") +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.09,  exp((-66.77) * (0.09 - 0.0718) - 7.119)), colour= "dark grey") +
    ylab("hazard") + xlab("age in years"),
  
  ggplot() + xlim(15, 105) + ylab("hazard (log scale)") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.025, exp((-66.77) * (0.025 - 0.0718) - 7.119))), colour = "red") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.04,  exp((-66.77) * (0.04 - 0.0718) - 7.119))), colour= "blue") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.06,  exp((-66.77) * (0.06 - 0.0718) - 7.119))), colour= "green") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.09,  exp((-66.77) * (0.09 - 0.0718) - 7.119))), colour= "dark grey") +
    xlab("age in years"),
  
  ggplot() + xlim(15, 105) + ylab("density") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.025, exp((-66.77) * (0.025 - 0.0718) - 7.119)), colour = "red") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.04,  exp((-66.77) * (0.04 - 0.0718) - 7.119)), colour= "blue") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.06,  exp((-66.77) * (0.06 - 0.0718) - 7.119)), colour= "green") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.09,  exp((-66.77) * (0.09 - 0.0718) - 7.119)), colour= "dark grey") +
    xlab("age in years"),
  
  ggplot() + xlim(15, 105) + ylim(0, 1) +
    geom_function(fun = function(x) gomp_lx(x - 15, exp((-66.77) * (0.025 - 0.0718) - 7.119), 0.025),
                  aes(col = "\u03B2 = 0.025")) +
    geom_function(fun = function(x) gomp_lx(x - 15,  exp((-66.77) * (0.04 - 0.0718) - 7.119), 0.04), aes(col = "\u03B2 = 0.04")) +
    geom_function(fun = function(x) gomp_lx(x - 15,  exp((-66.77) * (0.06 - 0.0718) - 7.119), 0.06), aes(col = "\u03B2 = 0.06")) +
    geom_function(fun = function(x) gomp_lx(x - 15,  exp((-66.77) * (0.09 - 0.0718) - 7.119), 0.09), aes(col = "\u03B2 = 0.09")) +
    ylab("survival") + xlab("age in years") + scale_colour_manual(values = c("red","blue","green", "dark grey")) +
    theme(legend.position = c(-1.25, -0.5), legend.title = element_blank()),
  
  ncol = 2
)
