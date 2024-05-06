# beta model values
beta1 <- 0.025
beta2 <- 0.04
beta3 <- 0.06
beta4 <- 0.09

# hgompertz(x, shape, rate): 
# x = age, shape = beta value, rate = derived from Sasaki & Kondo 2016 fig. 1, 2
# rate values according Sasaki & Kondo 2016 fig. 1, line 6, 30
Sab <- -2.624
Sbb <- 0.0393
Ma <- -7.119
Mb <- 0.0718
M1 <- Sab * (beta1 - Mb) / Sbb + Ma
M2 <- Sab * (beta2 - Mb) / Sbb + Ma
M3 <- Sab * (beta3 - Mb) / Sbb + Ma
M4 <- Sab * (beta4 - Mb) / Sbb + Ma

gridExtra::grid.arrange (
  
  ggplot()  + xlim(15, 100) + ylim(0, 0.4) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.025, exp(M1)), 
                  aes(col = "\u03B2 = 0.025")) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.04, exp(M2)),
                  aes(col = "\u03B2 = 0.04")) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.06, exp(M3)), 
                  aes(col = "\u03B2 = 0.06")) +
    geom_function(fun = function(x) flexsurv::hgompertz(x - 15, 0.09, exp(M4)), 
                  aes(col = "\u03B2 = 0.9")) +
    ylab("hazard") + xlab("age in years") +
    theme_light() + 
    scale_colour_manual(values = c("red","blue","green", "dark grey")) +
    theme(legend.position = c(0.2, 0.7), legend.title = element_blank()),
  
  ggplot() + xlim(15, 105) +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.025, exp(M1))), colour = "red") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.04, exp(M2))), colour= "blue") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.06, exp(M3))), colour= "green") +
    geom_function(fun = function(x) log(flexsurv::hgompertz(x - 15, 0.09, exp(M4))), colour= "dark grey") +
    xlab("age in years") + ylab("hazard (log scale)") +
    theme_light(),
  
  ggplot() + xlim(15, 105) +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.025, exp(M1)), colour = "red") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.04, exp(M2)), colour= "blue") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.06, exp(M3)), colour= "green") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, 0.09, exp(M4)), colour= "dark grey") +
    xlab("age in years")  + ylab("density") +
    theme_light(),

  # gomp_lx() s. functions\helper_functions.R  
  ggplot() + xlim(15, 105) + ylim(0, 1) +
    geom_function(fun = function(x) gomp_lx(x - 15, exp(M1), 0.025), colour = "red") +
    geom_function(fun = function(x) gomp_lx(x - 15, exp(M2), 0.04), colour = "blue") +
    geom_function(fun = function(x) gomp_lx(x - 15, exp(M3), 0.06), colour = "green") +
    geom_function(fun = function(x) gomp_lx(x - 15, exp(M4), 0.09), colour = "dark grey") +
    ylab("survival") + xlab("age in years") +
    theme_light(),
  
  ncol = 2
) -> gompertz_plot

# Save the finished map object
ggsave(
  filename = "fig01_gompertz_plot.pdf",
  width = 8, height = 6,
  plot = gompertz_plot, 
  device = cairo_pdf,
  path = "documented"
)
