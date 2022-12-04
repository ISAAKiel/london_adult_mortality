# Login needed to retreave data from the Human Mortality Database
# https://mortality.org/

if (runCodeNew){
  login <- askYesNo(paste("Login for Human Mortality Database needed.",
                          "Do you want to proceed?", sep = "\n"),
                    default = FALSE)
  # get dx
  if (login){
    HMD_UK_result_1_year <- HMDHFDplus::readHMDweb("GBRTENW", "bltper_1x1", 
                                                   readline(prompt = "Enter username: "), 
                                                   readline(prompt="Enter password: "))
    
    # saves results in Rda-object
    save(HMD_UK_result_1_year, file = file.path(".", saveFileDir, "HMD_UK_result_1_year.Rda") )
  }
}
load(file.path(".", saveFileDir, "HMD_UK_result_1_year.Rda") )

HMD_UK_result_1_year_list <- list(
  ggplot(HMD_UK_result_1_year[which(HMD_UK_result_1_year$Year == 1841),]) + geom_line(aes(x = Age, y = mx)) +
    scale_y_continuous(trans='log10') + ylab("mx (log scale)") +
    annotate("rect", xmin = 7, xmax = 17, ymin = 0.004, ymax = 0.01, alpha = .1,fill = "blue"),
  ggplot(HMD_UK_result_1_year[which(HMD_UK_result_1_year$Year == 1841),], aes(x = Age, y = mx)) + geom_line() +
    xlim(7,17) + ylim(0.004, 0.01) +
    ylab("mx") + geom_point() + geom_segment(aes(x = 12, y = 0.007, xend = 12, yend = 0.0055),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "red")
)
