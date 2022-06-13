source("./lifetables_processing/geneva.R")
source("./lifetables_processing/English_Mortality.R")
source("./lifetables_processing/halley_Breslau.R")
source("./lifetables_processing/suessmilch.R")
source("./lifetables_processing/Medieval_England.R")
source("./lifetables_processing/Germany.R")
source("./lifetables_processing/blayo_france.R")
source("./lifetables_processing/germany_imhof.R")

comp_df <- rbind(London_result, Paris_result, halley_result, 
                 suessmilch_result, medieval_result, uelzen_result, blayo_result )
rownames(comp_df) <- NULL
cols.num <- c("year", "beta", "alpha")
comp_df[cols.num] <- sapply(comp_df[cols.num],as.numeric)

hist_lt <- list()
hist_lt[[1]] <-  ggplot(geneva_result, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") + ylim(0.02, 0.07) +
    geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    ggtitle("Geneva") +  theme(plot.title = element_text(hjust = 0.5))
hist_lt[[2]] <-    ggplot(eng_mort_result, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") + ylim(0.02, 0.07) +
    geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    ggtitle("England") +  theme(plot.title = element_text(hjust = 0.5))
hist_lt[[3]] <-    ggplot(subset(germany_result, group == "gesamt"), aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") +
    geom_smooth(method='loess', span = 0.5, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
    ggtitle("Germany (selected rural regions, pooled)") + xlim(1740, 1850) + ylim(0.02, 0.07)
hist_lt[[4]] <-    ggplot(subset(germany_result, group == "hamburg"), aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") +
  geom_smooth(method='loess', span = 0.5, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
  ggtitle("Hamburg") + xlim(1740, 1850) + ylim(0.0, 0.07)
hist_lt[[5]] <-    ggplot(comp_df, aes(x = year, y = beta, group = group, colour = group, label = names ) ) + geom_point() + 
    ylab("Gompertz \u03B2") + xlab("year") + ggrepel::geom_text_repel(data = comp_df[comp_df$group != "France",]) + ylim(0.0, 0.07) + xlim(1400,1800) +
    ggtitle("Miscellaneous") +  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = c(1.25, 0.5))
