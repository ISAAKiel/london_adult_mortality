# data after Landers 1993, 41; 179 Table 5.7
# Finlay/Sheraer 1986, 39 table 1
# Encyclopedia of London, s. v. Population p. 655-657
# https://www.visionofbritain.org.uk/data_cube_page.jsp?data_theme=T_POP&data_cube=N_TOT_POP&u_id=10097836&c_id=10001043&add=N
date <- c(1100, 1200, 1300, 1350, 1500, 1550, 1600, 1650, 1695, 1730, 1740, 1750, 1760, 1770, 1780, 1790, 1801, 1811, 1821, 1831, 1841, 1851, 1861)
pop <- c(16000, 22500, 90000, 37500, 75000, 120000, 200000, 375000, 527560, 660000, 670000, 680000, 730000, 780000, 820000, 910000, 1096784, 1303564, 1573210, 1878229, 2207653, 2651939, 3188485)

date_diff <- date[-1] - date[-length(date)]
#size_diff <- size[-1] / size[-length(size)] # 
# rate <- 10^(log10(size_diff) / date_diff) # wrong calculation

# annual rate = ((SizeEnd/SizeStart) ^ (1/years))  - 1
rate_p_a <- ((pop[-1] / pop[-length(pop)]) ^ 
               (1 / (date[-1] - date[-length(date)])))-1

# year_date <- cumsum(date_diff) + 1200 # unused var

london_pop <- data.frame(date, pop, rate_p_a = c(NA, rate))

london_pop1 <- ggplotGrob(ggplot(london_pop, aes(x = date, y =pop/1000)) + 
                            geom_line() + geom_point(colour = "dark grey") +
                            scale_y_continuous(trans='log10') + 
                            ylab("population in thousand\n(log scale)") + 
                            xlim(1100, 1850) +
                            theme(axis.text = element_text(size = 12), 
                                  axis.title = element_text(size = 15)) + 
                            xlab("year AD"))
london_pop2 <- ggplotGrob(ggplot(london_pop, aes(x = date, y = rate_p_a )) + 
                            geom_bar(stat='identity') +
                            ylab("population increase (% p.a.) \n since last zensus") + 
                            xlab("year AD") + xlim(1100, 1850) +
                            theme(axis.text = element_text(size = 12), 
                                  axis.title = element_text(size = 15)))

# yearly rates of population increase for averaging
london_df <- data.frame()
for (i in 1:(length(date)- 1)) {
  year_rate <- data.frame(year = seq(date[i]+1, date[i + 1], 1), rate_p_a = rate_p_a[i])
  london_df <- rbind(london_df, year_rate)
}

# re-calculation of rates for Razzell/Spence 2007
razz_date <- c(1520 , 1600, 1650, 1700, 1750, 1801, 1851)
razz_pop <- c(55000, 200000, 400000, 575000, 675000, 960000, 2685000)
# razz_date_diff <- razz_dates[-1] - razz_dates[-length(razz_dates)]
# razz_size_diff <- razz_pop[-1] / razz_pop[-length(razz_pop)]
# razz_rate <- 10^(log10(razz_size_diff) / razz_date_diff)
razz_rate_p_a <- ((razz_pop[-1] / razz_pop[-length(razz_pop)]) ^ 
                    (1 / (razz_date[-1] - razz_date[-length(razz_date)])))-1
