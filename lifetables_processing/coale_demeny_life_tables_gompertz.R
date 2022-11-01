sex <- c("F", "M")
# x_vec <- demogR::cdmltw("F")$age[-c(1,2, 3, 4)] - 15
x_vec <- seq(from = 0, to = 80, by = 5)
# x_vec2 <- demogR::cdmltw("F")$age[-c(1,2, 3, 4)] + 5 - 15
x_vec2 <- seq(from = 5, to = 85, by = 5)
gompertz_df <- data.frame(i = NA, Gompertz_shape = NA, Gompertz_rate = NA, mx_15 = NA)
#Frage: Warum die vier Himmelsrichtungen?
for (s in sex) {
  lt_cdmltw <- demogR::cdmltw(s)
  lt_cdmlte <- demogR::cdmlte(s)
  lt_cdmlts <- demogR::cdmlts(s)
  lt_cdmltn <- demogR::cdmltn(s)
  lt_cd_all <- list(lt_cdmltw = lt_cdmltw, lt_cdmlte = lt_cdmlte, lt_cdmlts = lt_cdmlts, lt_cdmltn = lt_cdmltn)
  for (t in 1:length(lt_cd_all)) {
    for (v in 1:25) {
      dx_vec <- round(lt_cd_all[[t]]$ndx[v,-(1:4)] * 100000)#[v,]
      mx_15 <- lt_cd_all[[t]]$nmx[v,5]
      # das folgende passt nicht wegen unterschiedlicher Vektorlänge
      mort_df <- data.frame(x_vec, x_vec2, dx_vec)
      # C_D_Gomp <-  flexsurv::flexsurvreg(formula = survival::Surv(x_vec) ~ 1,
      #                                    weights = dx_vec, data = mort_df, dist="gompertz")
      # Gompertz_shape <- C_D_Gomp$coefficients[1]
      # Gompertz_rate <- exp(C_D_Gomp$coefficients[2])
      
      MLE_lt <- Gomp.MLE.interval(mort_df, agebegin = "x_vec", ageend = "x_vec2", Dx = "dx_vec")
      Gompertz_shape <- MLE_lt[2]
      Gompertz_rate <- MLE_lt[1]
      
      lt_name <- names(lt_cd_all[t])
      id <- paste(s, lt_name, v, sep="_")
      gompertz_df <- rbind(gompertz_df, c(id, Gompertz_shape, Gompertz_rate, mx_15))
    }
  }
}
gompertz_df <- gompertz_df[-1,]
cols.num <- c("Gompertz_shape", "Gompertz_rate", "mx_15")
gompertz_df[cols.num] <- sapply(gompertz_df[cols.num],as.numeric)
gompertz_df$M <- 1 / gompertz_df$Gompertz_shape * log (gompertz_df$Gompertz_shape/gompertz_df$Gompertz_rate) + 15
# ggplot(gompertz_df) + geom_point(aes(x = Gompertz_shape, y = (Gompertz_rate)))
# ggplot(gompertz_df) + geom_point(aes(x = mx_15, y = (Gompertz_rate)))
# ggplot(gompertz_df) + geom_histogram(aes(x = (Gompertz_shape)))
# ggplot(gompertz_df) + geom_density(aes(x = (M)))
# subset(gompertz_df, Gompertz_shape < 0.04)