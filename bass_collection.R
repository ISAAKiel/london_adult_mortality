path1 <- "/Volumes/SanDisk/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Milner_Boldsen_2011/all\ methods_milner\ boldsen.xlsx"
path2 <- "/Volumes/SanDisk/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Milner_Boldsen_2011/Sacroilliac_joint_milner\ boldsen.xlsx"
path3 <- "/Volumes/SanDisk/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Milner_Boldsen_2011/Pubic_symphysis_milner\ boldsen.xlsx"
path4 <- "/Volumes/SanDisk/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/Milner_Boldsen_2011/cranial_sutures_milner\ boldsen.xlsx"

paths <- c(path1, path2, path3, path4)
path_names <- c("all methods", "Sacroilliac joint", "Pubic symphysis", "cranial sutures")

bass <- as.data.frame(readxl::read_excel(path1, sheet = 1))

gomp.known_age(bass, known_age = "act",
               silent.jags = TRUE,
               silent.runjags = TRUE,
               thinSteps = 1,
               numSavedSteps = 10000,
               minimum_age = 15) %>%
  diagnostic.summary(., HDImass = 0.95) -> gomp_known_age_MCMC_diag
bayes_gomp_b <- gomp_known_age_MCMC_diag[2,5]
bayes_gomp_a <- gomp_known_age_MCMC_diag[1,5]

bayes_anthr_gomp_b <- NA
bayes_anthr_gomp_a <- NA
for (i in 1:4) {
  bass_anthro <- as.data.frame(readxl::read_excel(paths[i], sheet = 1))
  
  gomp.anthr_age(bass_anthro, age_beg = "min", age_end = "max",
                 silent.jags = TRUE,
                 silent.runjags = TRUE,
                 thinSteps = 1,
                 numSavedSteps = 10000,
                 minimum_age = 15) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  bayes_anthr_gomp_b[i] <- gomp_anthr_MCMC_diag[2,5]
  bayes_anthr_gomp_a[i] <- gomp_anthr_MCMC_diag[1,5]

}

bass_list <- list()
for(j in (1:4)){
bass_list[[1]] <- ggplot() + xlim(15, 110) + geom_density(data = bass, aes(x=act), bw=5, colour = "grey") +
  geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_anthr_gomp_b[j], bayes_anthr_gomp_a[j]), colour = "red") +
  geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_gomp_b,  bayes_gomp_a), colour= "blue") +
  labs(x = "age", y = "density") + ggtitle(path_names[j])
}

gridExtra::grid.arrange(
  ggplot() + xlim(15, 110) + geom_density(data = bass, aes(x=act), bw=5, colour = "grey") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_anthr_gomp_b[1], bayes_anthr_gomp_a[1]), colour = "red") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_gomp_b,  bayes_gomp_a), colour= "blue") +
    labs(x = "age", y = "density") + ggtitle(path_names[1]),
  ggplot() + xlim(15, 110) + geom_density(data = bass, aes(x=act), bw=5, colour = "grey") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_anthr_gomp_b[2], bayes_anthr_gomp_a[2]), colour = "red") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_gomp_b,  bayes_gomp_a), colour= "blue") +
    labs(x = "age", y = "density") + ggtitle(path_names[2]),
  ggplot() + xlim(15, 110) + geom_density(data = bass, aes(x=act), bw=5, colour = "grey") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_anthr_gomp_b[3], bayes_anthr_gomp_a[3]), colour = "red") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_gomp_b,  bayes_gomp_a), colour= "blue") +
    labs(x = "age", y = "density") + ggtitle(path_names[3]),
  ggplot() + xlim(15, 110) + geom_density(data = bass, aes(x=act), bw=5, colour = "grey") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_anthr_gomp_b[4], bayes_anthr_gomp_a[4]), colour = "red") +
    geom_function(fun = function(x) flexsurv::dgompertz(x - 15, bayes_gomp_b,  bayes_gomp_a), colour= "blue") +
    labs(x = "age", y = "density") + ggtitle(path_names[4]),
  bottom = "grey = density of actual ages (bandwidth = 5)\n blue = Gompertz distribution of actual ages\n red = Gompertz distribution of osteological estimates",
  ncol = 2
)

gomp_anthr_MCMC_diag

ggplot(bass) + geom_density(aes(x = act))
ggplot(bass) + geom_density(aes(x = max_likely))

density(bass$act)$x[which.max(density(bass$act)$y)]