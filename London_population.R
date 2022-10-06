# data after Landers 1993, 41; 179 Table 5.7
# Finlay/Sheraer 1986, 39 table 1
# https://www.visionofbritain.org.uk/data_cube_page.jsp?data_theme=T_POP&data_cube=N_TOT_POP&u_id=10097836&c_id=10001043&add=N
date <- c(1200, 1300, 1350, 1500, 1550, 1600, 1650, 1695, 1730, 1740, 1750, 1760, 1770, 1780, 1790, 1801, 1811, 1821, 1831, 1841, 1851)
size <- c(22500, 90000, 37500, 75000, 120000, 200000, 375000, 527560, 660000, 670000, 680000, 730000, 780000, 820000, 910000, 1090078, 1294765, 1560419, 1862970, 2185804, 2630782)

date_diff <- date[-1] - date[-length(date)]
size_diff <- size[-1] / size[-length(size)]
rate <- 10^(log10(size_diff) / date_diff)

london_pop <- data.frame(date, size, rate = c(NA, rate))

london_pop_list <- list(
  ggplot(london_pop, aes(x = date, y =size/1000)) + geom_line() + geom_point(colour = "dark grey") +
    scale_y_continuous(trans='log10') + ylab("population in thousand\n(log scale)") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) + xlab("year AD"),
  ggplot(london_pop, aes(x = date, y = (rate - 1) * 100) ) + geom_line() + geom_point(colour = "dark grey") +
    ylab("population increase\nper year in per cent") + xlab("year AD") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15))
)
