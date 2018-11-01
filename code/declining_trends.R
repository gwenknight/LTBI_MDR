## Declining trends? 
library(ggplot2)
library(magrittr)
library(xtable)
library(plyr)
library(dplyr)


load("~/Dropbox/MDR/output/all0_p_ds_mdr.Rdata")
cni <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv", stringsAsFactors = FALSE)[,-1]
theme_set(theme_bw(base_size = 24))

all10 <- subset(all0, year == 2010)
all14 <- subset(all0, year == 2014)
all01014 <- subset(all0, year > 2010)

coefs <- c()
for(i in 1:138){
  cnn <- cni[i]
  all <- all01014[which(all01014$iso3 == cnn),]
  for(j in 1:200){
    allt <- all[which(all$replicate == j), ]
    lm.t <- lm(allt$mdr_ari ~ allt$year)
    coefs <- rbind(coefs, c(i,j,lm.t$coefficients[2]))
  }
}

coefs <- as.data.frame(coefs)
colnames(coefs) <- c("cn","rep","coef")
coefs$iso3 <- cni[coefs$cn]
write.csv(coefs, paste0("~/Dropbox/MDR/output/coef_trend.csv"))

coefsm <- coefs %>% group_by(cn,iso3) %>%
  summarise_at(c("coef"),funs(median)) 

w<-which(coefsm$coef < 0)
length(w)

dec <- coefsm[w,"iso3"]

ggplot(coefsm, aes(x=iso3, y = coef)) + geom_point()

## Top 30 HBC MDR
mdr30 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/top30_mdr_countries.csv")

intersect(dec, mdr30$iso3)
