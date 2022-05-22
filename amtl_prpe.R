amtl <- read.table(file.choose(), header=TRUE, sep="\t")
str(amtl)
medieval <- NULL

for (j in 1:length(amtl$id)) {
  year <- amtl$age_min[j]
    for (i in 1:amtl$age_diff[j]) {
    ind_year <- NULL
    ind_year<-c(amtl$id[j], amtl$sex_loose[j], amtl$AmtlFreq[j], amtl$weight[j], year, (floor(year/5))*5)
    medieval <- rbind(medieval, ind_year)
    year <- year + 1
  }
}
colnames(medieval) <- c("id","sex_loose","AmtlFreq","weight","year", "year_cat")


str(medieval)
ind_year
head(medieval)
medieval <- as.data.frame.matrix(medieval)
library(magrittr)

medieval$id %<>% factor

medi_agg <-aggregate(medieval$weight, by=list(medieval$id, medieval$sex_loose, 
                                       medieval$AmtlFreq,medieval$year_cat), 
                    FUN=sum)
head(medi_agg)
colnames(medi_agg) <- c("id","sex_loose","AmtlFreq","year_cat","weight")
str(medi_agg)
medi_agg <- medi_agg[order(medi_agg$id),] 
medi_agg <- medi_agg[order(medi_agg$year_cat),] 

medi_avg<-NULL
for (k in unique(medi_agg$year_cat)) {
  medi_agg_sub <- subset(medi_agg, medi_agg$year_cat == k)
  wtd_mean <- wtd.mean(medi_agg_sub$AmtlFreq, weights=medi_agg_sub$weight)
  wtd_std <-  sqrt(wtd.var(medi_agg_sub$AmtlFreq, weights=medi_agg_sub$weight,normwt=FALSE))
    wtd_cat<-c(k, wtd_mean, wtd_std)
    medi_avg <- rbind(medi_avg, wtd_cat)
  }
medi_avg
