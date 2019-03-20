### Response to reviewers: new curves

## Libraries / home
library(ggplot2); library(gdata);library(RColorBrewer); library(data.table); library(magrittr); library(dplyr); library(plyr); library(maps)
library(rworldmap); library("TTR");library(tidyverse)
home <- "~/Documents/LTBI_MDR/"
output <- "~/Documents/LTBI_MDR/output/"

output <- "~/Dropbox/MDR/output" # TEMPORARY

## WHO data
data <- read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1]

## plot
countries <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]

ggplot(data, aes(x=year_new, y = av_mdr_new_pcnt)) + geom_point() + facet_wrap(~iso3, scales = "free")

ggplot(subset(data, iso3 %in% c("CHN","USA","IND","HKG")), aes(x=year_new, y = av_mdr_new_pcnt)) + geom_point() + facet_wrap(~iso3)

## Add in new metrics

## MUST HAVE ENOUGH after 2000
w<-which(data$year_new > 2000)
datan <- data %>% subset(year_new > 2000) %>% group_by(iso3) %>% dplyr::summarise(N10 = n()) ## ONLY TAKE IF > 5?

w <- which(datan$N10 > 5)

country_suff_after_2000 = unique(datan[w,"iso3"]) # 45 countries

## PEAK
datan <- data %>% subset(iso3 %in% country_suff_after_2000$iso3) %>% group_by(iso3) %>% 
                            dplyr::summarise(Max = max(av_mdr_new_pcnt), # Max value
                                             TMax = year_new[which.max(av_mdr_new_pcnt)] # When max value
                                             )

# max at time < 2010
dim(unique(datan[which(datan$TMax < 2010),"iso3"])) # 23... 
datan10 <- datan %>% subset(TMax < 2010)
datam <- merge(datan10,data, by = "iso3")

ggplot(datam, aes(x=year_new, y = av_mdr_new_pcnt)) + geom_point() + facet_wrap(~iso3, scales = "free")


## GRADIENT
datan <- datam %>% subset(year_new > 2010) %>% group_by(iso3) %>% dplyr::summarise(Grad = (max(av_mdr_new_pcnt) - min(av_mdr_new_pcnt))/5)
ggplot(datan, aes(iso3, Grad)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


d <- datam %>% 
  group_by(iso3) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(av_mdr_new_pcnt ~ year_new, data = .)))


 ## when max
which.max(d_usa$av_mdr_new_pcnt)



# moving avergae
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}




d_usa <- subset(data, iso3 == "USA")
plot.ts(SMA(d_usa$av_mdr_new_pcnt, n = 8))


mav(d_usa[,c("year_new","av_mdr_new_pcnt")],10)
plot(mav(d_usa[,c("year_new","av_mdr_new_pcnt")],4))

