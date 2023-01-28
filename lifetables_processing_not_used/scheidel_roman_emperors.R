path <- "./data/scheidel_roman_emperors.txt"
emperors <- read.table(path, header = TRUE, sep = "\t")
emperors$death <- emperors$years_lived + emperors$age_inthron
emperors$years_cat <- floor(emperors$death/5) * 5
emperors$years_lived <- emperors$death + 0.5 - emperors$years_cat
emp_seq <- seq(min(emperors$age_inthron), max(emperors$death),1 )
length_emp_seq <- length(emp_seq)
emp_numb <- length(emperors$name)
result_emp <- data.frame()
for(i in (1 : emp_numb)) {
  for (j in (1 : length_emp_seq)) {
    result_seq <- NULL
    if (emp_seq[j] >= emperors$age_inthron[i] & emp_seq[j] <= emperors$death[i]) {
      exp_bool <- 1
    } else {
      exp_bool <- 0
    }
    result_seq <- c(emperors$name[i], emp_seq[j], exp_bool)
    result_emp <- rbind(result_emp, result_seq)
  }
}
colnames(result_emp) <- c("name", "year","yes_no")
cols.num <- c("year","yes_no")
result_emp[cols.num] <- sapply(result_emp[cols.num],as.numeric)
exp_years <- result_emp %>% group_by(year_cat = floor(year/5) * 5) %>% summarize(expo = sum(yes_no))
death_years <- emperors %>% group_by(year_cat = floor(death/5) * 5) %>% summarize(death = n(), years_lived_cat = sum(years_lived))
emp_lt <- merge(death_years, exp_years)

emp_lt$nax <- emp_lt$years_lived_cat / emp_lt$death
emp_lt$mx <- emp_lt$death / emp_lt$expo
emp_lt$qx <- 5 * emp_lt$mx / (1 + (5 - 2.5) * emp_lt$mx)

ggplot(emp_lt) + geom_line(aes(x = year_cat, y = dx))

df_length <- length(emp_lt$qx)

# calculation of dx
dx <- NULL
lx_ <- NULL
lx <- 1
for (k in 1: df_length ) {
  dx_1 <- emp_lt$qx[k] * lx
  lx <- lx - dx_1
  dx <- c(dx, dx_1)
  lx_ <- c(lx_, lx)
}
emp_lt$dx <- dx

emp_lt$death <- 1
sample_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(year_cat-30, death) ~ 1, 
                                        data = emp_lt, dist="gompertz", weights = dx)
sample_data_lt_Gompertz_shape <- sample_data_lt$coefficients[1]
sample_data_lt_Gompertz_rate <- exp(sample_data_lt$coefficients[2])
sample_data_lt_Gompertz_shape
sample_data_lt_Gompertz_rate
