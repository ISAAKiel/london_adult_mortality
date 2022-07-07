source("./lifetables_processing/geneva.R")
source("./lifetables_processing/English_Mortality.R")
source("./lifetables_processing/halley_Breslau.R")
source("./lifetables_processing/suessmilch.R")
source("./lifetables_processing/Medieval_England.R")
source("./lifetables_processing/Germany.R")
source("./lifetables_processing/blayo_france.R")
source("./lifetables_processing/germany_imhof.R")

hist_lt <- list()
hist_lt[[1]] <-    ggplot(medieval_result, aes(x = year, y = beta, label = names ) ) + geom_point() + 
  ylab("Gompertz \u03B2") + xlab("year") + ylim(0.025, 0.075) + 
  ggtitle("Medieval England") +  theme(plot.title = element_text(hjust = 0.5)) +
  ggrepel::geom_text_repel()
hist_lt[[2]] <-  ggplot(geneva_result, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") + ylim(0.025, 0.075) +
    geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    ggtitle("Geneva") +  theme(plot.title = element_text(hjust = 0.5))
hist_lt[[3]] <-    ggplot(eng_mort_result, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") + ylim(0.025, 0.075) +
    geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    ggtitle("England") +  theme(plot.title = element_text(hjust = 0.5))
hist_lt[[4]] <-    ggplot(blayo_result, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") +
  #geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
  ggtitle("France") + xlim(1740, 1850) + ylim(0.025, 0.075) +  theme(plot.title = element_text(hjust = 0.5))
hist_lt[[5]] <-    ggplot(subset(germany_result_qx, group == "gesamt"), aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") +
    geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    ggtitle("Germany (rural)") + xlim(1740, 1850) + ylim(0.025, 0.075) +  theme(plot.title = element_text(hjust = 0.5))
hist_lt[[6]] <-    ggplot(subset(germany_result_qx, group == "hamburg"), aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") +
  geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
  ggtitle("Hamburg") + xlim(1740, 1850) + ylim(0.025, 0.075) +  theme(plot.title = element_text(hjust = 0.5))