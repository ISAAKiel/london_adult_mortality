# Login needed to retrieve data from the Human Mortality Database
# https://mortality.org/

if (runCodeNew){
  login <- askYesNo(paste("Login for Human Mortality Database needed.",
                          "Do you want to proceed?", sep = "\n"),
                    default = FALSE)
  # get dx
  if (login){
    HMD_UK_result_1_year <- HMDHFDplus::readHMDweb("GBRTENW", "bltper_1x1", 
                                                   credentials[1], 
                                                   credentials[2])
    
    # saves results in Rda-object
    save(HMD_UK_result_1_year, file = file.path(".", saveFileDir, 
                                                "HMD_UK_result_1_year.Rda") )
  }
} else {load(file.path(".", saveFileDir, "HMD_UK_result_1_year.Rda") )
  }


gridExtra::grid.arrange(
  ggplot(HMD_UK_result_1_year[which(HMD_UK_result_1_year$Year == 1841),]) + 
    geom_line(aes(x = Age, y = mx)) +
    scale_y_continuous(trans='log10') + labs(y = expression(m[x] * " (log scale)")) +
    annotate("rect", xmin = 7, xmax = 17, ymin = 0.004, ymax = 0.01, 
             alpha = .1,fill = "blue") +
    annotate (geom = "text", x = 12, y = 0.012, label = "zoom") +
    theme_light(),
  ggplot(HMD_UK_result_1_year[which(HMD_UK_result_1_year$Year == 1841),], 
         aes(x = Age, y = mx)) + scale_x_continuous(breaks=seq(8,16,2), 
                                                    limits=c(7, 17))  + 
    geom_line() + geom_point() +
    ylim(0.004, 0.01) +
    labs(y = expression(m[x]))+
    annotate (geom = "text", x = 10, y = 0.009, label = "zoomed in") +
    geom_segment(aes(x = 12, y = 0.007, xend = 12, yend = 0.0055),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "red") +
    theme_light(),
  ncol = 1
) -> HMD_UK_hazard_plot

# Save the finished map object
ggsave(
  filename = "fig03_HMD_UK_hazard_plot.pdf",
  plot = HMD_UK_hazard_plot, 
  device = "pdf",
  path = "documented"
)
