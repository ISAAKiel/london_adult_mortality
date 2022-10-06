# Data from Birch 1759
if (runCodeNew){
  set.seed(745)
  london_1758 <- data.frame(age = seq(0, 94, 1), lx = c(1000, 750, 637, 597, 570, 550, 540, 531, 525, 520, 516, 512, 509, 506, 504, 502, 500, 497, 494, 490, 485, 478, 471, 464, 456, 448, 440, 432, 424, 416, 408, 399, 390, 381, 372, 362, 352, 342, 332, 322, 312, 302, 292, 282, 272, 262, 252, 242, 233, 224, 215, 206, 197, 189, 181, 173, 165, 157, 149, 142, 135, 128, 121, 114, 108, 102, 96, 90, 84, 78, 72, 67, 62, 57, 52, 47, 43, 39, 35, 31, 27, 24, 21, 18, 15, 12, 10, 8, 6, 5, 4, 3, 2, 1, 0))
  
  df_length <- length(london_1758$age)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- london_1758$lx[k] - london_1758$lx[k + 1]
    dx <- c(dx, dx_1)
  }
  london_1758$dx <- dx
  
  london_1758_uncount <- london_1758[-c(1:15, 95),-2] %>% uncount(dx)
  
  gomp.known_age(london_1758_uncount, known_age = "age",
                 thinSteps = 1,
                 numSavedSteps = 200000, minimum_age = 15) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_known_age_MCMC_diag
  
  London_1758_result <- gomp_known_age_MCMC_diag
  
  # saves results in Rda-object
  save(London_1758_result, file = file.path(".", saveFileDir, "London_1758_result.Rda") )
}
load(file.path(".", saveFileDir, "London_1758_result.Rda") )

# mode values
modes <- London_1758_result[2:3,]$Mode
London_1758_20ex <- gomp.ex(20, London_1758_result[1,5], London_1758_result[2,5])
London_1758_25ex <- gomp.ex(25, London_1758_result[1,5], London_1758_result[2,5])

# range of Gompertz beta values
beta_range <- paste0(format(round(London_1758_result[2,]$HDIlow, digits = 4), nsmall = 4 ), "-",
                     format(round(London_1758_result[2,]$HDIhigh, digits = 4), nsmall = 4  ) )
# range of age modes M
M_range <- paste0(format(round(London_1758_result[3,]$HDIlow, digits = 1), nsmall = 1 ), "-",
                  format(round(London_1758_result[3,]$HDIhigh, digits = 1), nsmall = 1 ) )

London_1758_ranges <- data.frame(parameter = c("beta", "M", "e20", "e25"), 
                                 modes = format(round(c(modes,London_1758_20ex, London_1758_25ex), 4), nsmall = 4), 
                                 HDI.ranges = c(beta_range, M_range, NA, NA))
#ggplot(london_1758[-c(1:15),]) + geom_line(aes(x = age, y = dx))

London_1758_prep <- data.frame(source = "written", data = "London 1728-57", M = London_1758_result[3,5], start = 1728, end = 1757, 
                         year = NA, HDIlow = NA, HDIhigh = NA)
London_1758_beta_prep <- data.frame(source = "written", data = "London 1728-57", beta = London_1758_result[2,5], start = 1728, end = 1757, 
                               year = NA, HDIlow = NA, HDIhigh = NA)
