# St. Brides

infotext <- paste ("Data available from the Museum of London upon request.",
                   "https://www.museumoflondon.org.uk",
                   "Please download the data and edit the code.",
                   sep="\n")

stop(infotext)

# Data 
molas_data <- # 'path/to/downloaded/excel.xls'

  options(dplyr.summarise.inform = FALSE)
my_data1 <- readxl::read_excel(molas_data, sheet = 1)
my_data2 <- readxl::read_excel(molas_data, sheet = 2)
my_data3 <- readxl::read_excel(molas_data, sheet = 3) [, 1:8]
merged <- bind_rows(my_data1,my_data2)
colnames(merged) <- c("site", "ind", "sex", "age", "tp", "tooth")
colnames(my_data3) <- c("site", "ind", "birth", "death", "known_sex", "known_age", "sex", "age")
merged_sub <- merged
ind_list <- merged_sub %>% group_by(ind, sex, age) %>% summarize(n())
ind_list <- ind_list[,-c(2,3)]
ind_amtl <- merged_sub %>% group_by(ind) %>% filter(tooth == 3) %>% summarize(n())
ind_merged <- merge(x=ind_list, y = ind_amtl, by= "ind", all = TRUE)
colnames(ind_merged) <-c("ind", "tp", "amtl")
ind_merged[is.na(ind_merged)] <- 0
stbrides <- merge(x=my_data3, y = ind_merged, by= "ind", all = FALSE)
stbrides$known_age <- as.integer(stbrides$known_age)
stbrides <- na.omit(stbrides)
stbrides <- subset(stbrides, known_age >= 15 & ind != 56)
str(stbrides)

stbrides <- stbrides %>% mutate (age_beg_w =
  ifelse(age == 6, 15,
  ifelse(age == 7, 18,
  ifelse(age == 8, 26,
  ifelse(age == 9, 36,
         ifelse(age == 10, 46, 18
  ) ) ) ) ) )
stbrides <- stbrides %>% mutate (age_end_w =
                        ifelse(age == 6, 17,
                               ifelse(age == 7, 25,
                                      ifelse(age == 8, 35,
                                             ifelse(age == 9, 45, 99
                                             ) ) ) ) )

stbrides %>%
  mutate(death=1) %>%
  flexsurv::flexsurvreg(formula = Surv(known_age - 15, death) ~ 1,
                        data = .,
                        dist="gompertz") -> stbridges_ind_mle_gompertz

hk.tot <- structure(c(15,20,35,50,20,35,50,Inf,50,166,88, 24),
                    .Dim=as.integer(c(4,3)),Dimnames=list(NULL, c("col1","col2","col3")))
stbrides_strip <- data.frame(stbrides$age_beg_w, stbrides$age_end_w, death = 1)

Gomp_MLE_int <- function(x, deaths = stbrides_strip){
  alpha = x[1]
  beta = x[2]
  shift<- 15 # only ages 15 and up are considered
  nrow <- NROW(deaths)
  S.t<- function(t){
    return(exp(alpha/beta*(1-exp(beta*(t-shift)))))
  }
  d <- S.t(deaths[1:nrow,1])-S.t(deaths[1:nrow,2])
  obs <- deaths[,3]
  lnlk <- as.numeric(crossprod(obs,log(d)))
  return(lnlk)
}
optim(c(0.01, 0.01), Gomp_MLE_int, control = list(fnscale=-1))
