path <- "/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Imhof/Dx"
filenames <- list.files(path, full.names=FALSE)
filenames <- sub("_.*", "", filenames)
filepaths <- list.files(path, full.names=TRUE)
file_nr <- length(filenames)
germany <- data.frame()
for (i in 1:file_nr) {
  germany_lt <- read.table(filepaths[i], header=TRUE, sep = "\t")
  germany_lt_melt <- reshape2::melt(germany_lt, id.vars = "X", value.name = "Dx")
  germany_lt_melt$sample <- filenames[i]
  germany <- rbind(germany, germany_lt_melt)
}
germany$year <- substr(germany$variable, 2, 5)
germany <- germany[,-2]
colnames(germany) <- c("age", "Dx", "sample", "year")
germany_sub <- subset(germany, age >= 15)

germany_result <- data.frame()
for(i in filenames) {
  sample_data <- germany_sub[ which(germany_sub$sample == i), ]
  years <- unique(sample_data$year)
  for (j in years) {
    sample_data_year <- sample_data[which(sample_data$year == j), ]
    
    sample_data_year$death <- 1
    sample_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age - 15, death) ~ 1, 
                                            data = sample_data_year, dist="gompertz", weights = Dx)
    sample_data_lt_Gompertz_shape <- sample_data_lt$coefficients[1]
    sample_data_lt_Gompertz_rate <- exp(sample_data_lt$coefficients[2])
    
    ind_result <- cbind(group = i, year = j, 
                        beta = sample_data_lt_Gompertz_shape, alpha = sample_data_lt_Gompertz_rate)
    germany_result <- rbind(germany_result, ind_result )
  }
}
rownames(germany_result) <- NULL
cols.num <- c("year", "beta", "alpha")
germany_result[cols.num] <- sapply(germany_result[cols.num],as.numeric)

# plot_list <- list()
# for (j in filenames) {
#   group_data <- germany_result[ which(germany_result$group == j), ]
#   plot_list[[j]] <- ggplot(group_data, aes(x = year, y = beta)) + geom_point() + ylab("Gompertz \u03B2") +
#     geom_smooth(method='loess', span = 0.75, formula = y ~ x, colour = "red", se = TRUE, level = 0.95) +
#     xlab(j) + xlim(1680, 1860) + ylim(0.0, 0.07)
# }
# do.call(gridExtra::grid.arrange, c(plot_list, ncol = 3))
