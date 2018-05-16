### Run cohort_ltbi_mdr
library(ggplot2)
library(plyr)
setwd("~/Documents/LTBI_MDR/")

# ARI DS
# Got for India from similar to this: 
#source("code/postARIanalysis.R")
#w<-which(rundata$iso3 == "IND")
#rundata_ind <-rundata[w,]
#rundata_ind$ari <- exp(rundata_ind$lari)
#save(rundata_ind,file="data/rundata_ind.Rdata")
load("data/rundata_ind.Rdata")
#ggplot(rundata_ind, aes(x=year,y=ari,col=replicate)) + geom_point()

ari <- as.data.frame(matrix(0,81,2))
colnames(ari) <- c("ds","mdr")
#ari$ds <- seq(10^(-4),10^(-5),length=81)
#ari$ds <- rundata_ind[,mean(ari),by = "year"][,2]
ari$ds <- 0.01

## India 2016.perc_new_mdr = ~2%
#ari$mdr <- c(rep(0,1970-1934),seq(0,0.02,length=(2014-1969))) * ari$ds
ari$mdr <- 0
colnames(ari) <- c("ds","mdr")

# Population model
load('data/POP2014.Rdata')  
w<-which(POP2014$area == "India")
pop_ind <- POP2014[w,]

### Run 
cc <- cohort_ltbi(ari, pop_ind$value)

combs <- cc$combs
ltbi_dr <- sum(combs$perc_dr)
ltbi_ds <- sum(combs$perc_ds)
ltbi_dr
ltbi_ds


plot(perc_ds_ind)
points(perc_dr_ind,col="red")
sigm <- function(x,delta){1970 + 1/(1+exp(-(x-delta)))}
plot(seq(1970,2030,1),sigm(seq(0,60,1),20))


