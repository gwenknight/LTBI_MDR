## Try to generate figures as in Houben&Dodd

setwd("~/Documents/LTBI_MDR/")

### Replicate figures for all TB
source("code/postARIanalysis.R")

## for one country... 
w<-which(bdzdf$iso3 == "THA")
bi <- bdzdf[w,]
Alli<-All[which(All$iso3 == "THA"),]

ggplot(bi, aes(x=year,y=lari)) + geom_line(col="red") + geom_line(aes(x=year,y=upper),col="red",linetype = 2) +
  geom_line(aes(x=year,y=lower),col="red",linetype = 2) + 
  geom_point(data = Alli, aes(x=year,y=lari,shape=type,col=type)) + geom_errorbar(data = Alli, aes(ymin = lari-E, ymax = lari+E))

### Replicate figures for MDR-TB
source("code/postARIanalysis_mdr.R")

## for one country... 
w<-which(bdzdf$iso3 == "THA")
bi <- bdzdf[w,]
Alli<-All[which(All$iso3 == "THA"),]

ggplot(bi, aes(x=year,y=lari)) + geom_line(col="red") + geom_line(aes(x=year,y=upper),col="red",linetype = 2) +
  geom_line(aes(x=year,y=lower),col="red",linetype = 2) + 
  geom_point(data = Alli, aes(x=year,y=lari,shape=type,col=type)) + geom_errorbar(data = Alli, aes(ymin = lari-E, ymax = lari+E))
