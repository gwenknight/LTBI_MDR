###### Sort out Cohen rough

setwd("~/Dropbox/MRC SD Fellowship/Research/MDR/data/")

d <- read.csv("cohen_rough_2014.csv")
is.numeric("france")


for(i in 1:length(d)){
  if(is.numeric(d[i,1])){
    
  }
  
  
}

#d <-strsplit(readLines("rcohen.txt"), "[[:space:]]+")[[1]]
options(stringsAsFactors = FALSE)

r <- readLines("rcohen.txt",n=2000)
rr <- paste(r,collapse = " ")
d <- strsplit(rr,"[[:space:]]+")[[1]]

w<- which(is.na(as.numeric(d))) # location of names

w1<- w[c(TRUE,diff(w)!=1)] # start of location of names


rem <- c() # those to remove at the end from d
for(i in 1:length(w1)){
  print(i)
  s = 0; cc<-""
  j = w1[i]
  jn = w1[i]
  while (s == 0){
    print(jn)
    if(!is.na(as.numeric(d[jn]))){s = 1}else{
      cc <- paste(cc,d[jn],sep=" ");
      jn = jn + 1}
  }
  jn = jn - 1
  if(jn > j){
    d[w1[i]] <- cc
    rem <- c(rem,(j+1):(jn)) 
  }
}
d <- d[-rem] # remove the unnecessary strings
countries <- d[seq(1,length(d),4)]
year <- d[seq(2,length(d),4)]
num <- d[seq(3,length(d),4)]
mdr_num <- d[seq(4,length(d),4)]


dt <- as.data.frame(cbind(countries, as.numeric(year), as.numeric(num), as.numeric(mdr_num)))
colnames(dt) <- c("country","year","test_num","mdr_num")
dt$prop_mdr <- as.numeric(dt$mdr_num) / as.numeric(dt$test_num)
dt$prop_mdr_sd <- sqrt(abs(dt$prop_mdr * (1-dt$prop_mdr)/as.numeric(dt$test_num)))
write.csv(dt,"Cohen_2014_tableA1.csv")

#### Plots
library(ggplot2)

ggplot(dt, aes(x = year, y = prop_mdr, label = country)) + 
  geom_point() + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + 
  scale_y_continuous("Proportion MDR")

ggplot(dt, aes(x = year, y = 100*prop_mdr, label = country)) + 
  geom_point() + geom_text(aes(label=country),hjust = 0, vjust = 0) + 
  scale_y_continuous("Percentage MDR")

### Add in multiple column - don't need ! all multiple...
# u_countries <- unique(dt$country)
# dt$multiple <- 0
# for(i in 1:length(u_countries)){
#   w<-which(dt$country == u_countries[i])
#   if(length(w) > 1){dt[w,"multiple"] <- 1}
# }
# 
# ggplot(dt, aes(x = year, y = prop_mdr, label = country, colour = factor(multiple))) + 
#   geom_point() + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + 
#   scale_y_continuous("Proportion MDR") 


ggplot(dt, aes(x = year, y = prop_mdr, label = country, group = country)) + 
  geom_line() + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + 
  scale_y_continuous("Proportion MDR") 

ggplot(dt, aes(x = year, y = prop_mdr, label = country, group = country)) + 
  geom_line() + scale_y_continuous("Proportion MDR",trans='log10')  + facet_wrap(~ country, scales = "free")

#### ESTONIA
w <- which(dt$country == "Estonia")
ggplot(dt[w,], aes(x = year, y = prop_mdr, label = country, group = country)) + 
  geom_point() + scale_y_continuous("Proportion MDR",trans='log10', breaks = c(0,0.1,1))

mean(dt[w,"prop_mdr"])



###### Sort out WHO 2017
setwd("~/Dropbox/MRC SD Fellowship/Research/MDR/data/")


r <- readLines("who_2017.txt",n=2000)
rr <- paste(r,collapse = " ")
d <- strsplit(rr,"[[:space:]]+")[[1]]

w<- which(is.na(as.numeric(d))) # location of names

w1<- w[c(TRUE,diff(w)!=1)] # start of location of names


rem <- c() # those to remove at the end from d
for(i in 1:length(w1)){
  print(c("i",i))
  s = 0; cc<-""
  j = w1[i]
  jn = w1[i]
  while (s == 0){
    print(jn)
    if(!is.na(as.numeric(d[jn]))){s = 1}else{
      cc <- paste(cc,d[jn],sep=" ");
      jn = jn + 1}
  }
  jn = jn - 1
  if(jn > j){
    d[w1[i]] <- cc
    rem <- c(rem,(j+1):(jn)) 
  }
}
d <- d[-rem] # remove the unnecessary strings
countries <- d[seq(1,length(d),4)]
year <- d[seq(2,length(d),4)]
num <- d[seq(3,length(d),4)]
mdr_num <- d[seq(4,length(d),4)]


dt <- as.data.frame(cbind(countries, as.numeric(year), as.numeric(num), as.numeric(mdr_num)))
colnames(dt) <- c("country","year","test_num","mdr_num")
dt$prop_mdr <- as.numeric(dt$mdr_num) / as.numeric(dt$test_num)
dt$prop_mdr_sd <- sqrt(abs(dt$prop_mdr * (1-dt$prop_mdr)/as.numeric(dt$test_num)))
write.csv(dt,"Cohen_2014_tableA1.csv")

#### Plots
library(ggplot2)

ggplot(dt, aes(x = year, y = prop_mdr, label = country)) + 
  geom_point() + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + 
  scale_y_continuous("Proportion MDR")

ggplot(dt, aes(x = year, y = 100*prop_mdr, label = country)) + 
  geom_point() + geom_text(aes(label=country),hjust = 0, vjust = 0) + 
  scale_y_continuous("Percentage MDR")

### Add in multiple column - don't need ! all multiple...
# u_countries <- unique(dt$country)
# dt$multiple <- 0
# for(i in 1:length(u_countries)){
#   w<-which(dt$country == u_countries[i])
#   if(length(w) > 1){dt[w,"multiple"] <- 1}
# }
# 
# ggplot(dt, aes(x = year, y = prop_mdr, label = country, colour = factor(multiple))) + 
#   geom_point() + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + 
#   scale_y_continuous("Proportion MDR") 


ggplot(dt, aes(x = year, y = prop_mdr, label = country, group = country)) + 
  geom_line() + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + 
  scale_y_continuous("Proportion MDR") 

ggplot(dt, aes(x = year, y = prop_mdr, label = country, group = country)) + 
  geom_line() + scale_y_continuous("Proportion MDR",trans='log10')  + facet_wrap(~ country, scales = "free")

#### ESTONIA
w <- which(dt$country == "Estonia")
ggplot(dt[w,], aes(x = year, y = prop_mdr, label = country, group = country)) + 
  geom_point() + scale_y_continuous("Proportion MDR",trans='log10', breaks = c(0,0.1,1))

mean(dt[w,"prop_mdr"])
