# # data from Sasaki/Kondo 2016, 526 Tab. 1
# names <- c("Roman", "English peers", "Hida, Japan", "Denmark", "Geneva", "Agta", "Ache", "Hadza", "Hiwi", "!Kung", "Tsimane")
# years <- c(230, 1300, 1787, 1792,1637, 1957, 1971, 1987, 1960, 1969, 1970)
# group <- c("historic", "historic", "historic", "historic", "historic", "historic", 
#            "modern hunter-gatherers", "modern hunter-gatherers", "modern hunter-gatherers", "modern hunter-gatherers",
#            "modern hunter-gatherers", "modern hunter-gatherers")
# gomp_beta <- c(0.0589, 0.06078215, 0.0519, 0.0493, 0.0426, 0.0519, 0.0376, 0.0452, 0.0347, 0.0843, 0.0519)
#comp_df <- data.frame(names, group, years, gomp_beta)
eng_mort_sub <- data.frame(names = "English", group = "English", years = eng_mort_result$year, gomp_beta = eng_mort_result$beta)
comp_df <- rbind(eng_mort_sub, medieval_export, uelzen_df, 
                 c(names = "Paris", group = "historic", year = 1740, gomp_beta = Paris_lt_Gompertz_shape) )

ggplot(comp_df, aes(x = years, y = gomp_beta, group = group, colour = group, label = names ) ) + geom_point() + 
  ylab("Gompertz \u03B2") + xlab("years AD") + ggrepel::geom_text_repel(data = subset(comp_df, group != "English"))
