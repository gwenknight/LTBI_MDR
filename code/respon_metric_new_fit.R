### Response to reviewers: new curves

## Libraries / home
library(ggplot2); library(gdata);library(RColorBrewer); library(data.table); library(magrittr); library(dplyr); library(plyr); library(maps)
library(rworldmap); library("TTR");library(tidyverse);library(zoo);library(DataCombine);
library(lattice); library(lme4)
home <- "~/Documents/LTBI_MDR/"
output <- "~/Documents/LTBI_MDR/output/"

output <- "~/Dropbox/MDR/output" # TEMPORARY

## WHO data
data <- read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1]

## plot
countries <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]

# ALL
#ggplot(data, aes(x=year_new, y = av_mdr_new_pcnt)) + geom_point() + facet_wrap(~iso3, scales = "free")

# Just those mentioned by Reviewer / look odd
ggplot(subset(data, iso3 %in% c("CHN","USA","IND","HKG")), aes(x=year_new, y = av_mdr_new_pcnt)) + geom_point() + facet_wrap(~iso3)

### NEW FILTER
## MUST HAVE AT LEAST 5
datan <- data %>% group_by(iso3) %>% dplyr::summarise(N10 = n()) ## ONLY TAKE IF > 5?
# More than 5 data points 
w <- which(datan$N10 > 5)
country_at_least_5 = unique(datan[w,"iso3"]) 



# ## MUST HAVE ENOUGH after 2000 and 2010
# ## 2000
# datan <- data %>% subset(year_new > 2000) %>% group_by(iso3) %>% dplyr::summarise(N10 = n()) ## ONLY TAKE IF > 5?
# # More than 5 data points after 2000
# w <- which(datan$N10 > 5)
# country_suff_after_2000 = unique(datan[w,"iso3"]) 
# 
# ## 2010
# datan <- data %>% subset(year_new > 2009) %>% group_by(iso3) %>% dplyr::summarise(N10 = n()) ## ONLY TAKE IF > 1?
# # More than 1 data points after 2010
# w <- which(datan$N10 > 0)
# country_any_after_2010 = unique(datan[w,"iso3"]) 
# 
# country_suff = intersect(country_suff_after_2000, country_any_after_2010)
# length(country_suff$iso3) # 41 countries

country_suff <- country_at_least_5$iso3

## PEAK - take from moving average
data_ma = as.data.table(data %>% subset(iso3 %in% country_suff) %>% group_by(iso3) %>% 
  mutate(ma3 = rollmean(x = av_mdr_new_pcnt, 3, align = "center", fill = NA)))

data_ma[, ma3 := c(0, ma3[-c(1, .N)], 0)[1:.N], by = iso3] # first and last to zero in moving average

ggplot(data_ma, aes(x=year_new, y = av_mdr_new_pcnt)) + geom_point() + facet_wrap(~iso3, scales = "free") + 
  geom_vline(xintercept = 2010, col ="red") + geom_line(aes(y = ma3))

datan <- data_ma %>% group_by(iso3) %>% 
  dplyr::summarise(Max = max(ma3), # Max value
                   TMax = year_new[which.max(ma3)] # When max value
  )

# max at time < 2010
dim(unique(datan[which(datan$TMax < 2010),"iso3"])) # 23 
datan10 <- datan %>% subset(TMax < 2010)
datam <- merge(datan10,data_ma, by = "iso3")

ggplot(datam, aes(x=year_new, y = av_mdr_new_pcnt)) + geom_point() + facet_wrap(~iso3, scales = "free") + 
  geom_vline(xintercept = 2010, col ="red") + geom_line(aes(y = ma3)) + 
  geom_errorbar(aes(ymin = 100*mlo, ymax = 100*mhi))
setwd(output)
ggsave("countries_for_2sa.pdf")

uu <- unique(datam$iso3)
write.csv(uu, "countries_2sa.csv")

##### Contributing burden
## Top 30 HBC MDR
mdr30 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/top30_mdr_countries.csv")

uu[uu %in% mdr30$iso3]

# MDR TB burden 2016
mtb18 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/MDR_RR_TB_burden_estimates_2018-08-10.csv", stringsAsFactors = FALSE)
mtb18$mdr_inc_num <- mtb18$mdr_inc_num/1000

totalmtb18 <- sum(mtb18$mdr_inc_num) # 493804 = similar to 2017 GTB report
mtb18$perc_total <- 100*mtb18$mdr_inc_num / totalmtb18

x <- mtb18 %>% subset(iso3 %in% uu) 
x[which(x$perc_total > 0.1),"iso3"] # ONLY China and India

## MDR-LTBI burden
med <- read.csv("med_pop_ltbir.csv")
totalpltbir <- sum(med$pltbir)
xx <- med %>% subset(iso3 %in% uu)
xx$percltbir <- 100*xx$pltbir / totalpltbir
xx[which(xx$percltbir > 0.1),c("percltbir","iso3")] # China, India and USA

###### GRADIENT
dg <- datam %>% group_by(iso3) %>% arrange(iso3,year_new) %>% 
  do(tail(., 5)) %>% subset(year_new > 2004) # just fit to last 5 datapoints

fits <- lmList(av_mdr_new_pcnt ~ year_new | iso3, data=dg)
fits

data_ma = as.data.table(datam %>% subset(iso3 %in% country_suff$iso3) %>% group_by(iso3) %>%
                          mutate(ma5 = rollmean(x = av_mdr_new_pcnt, 5, align = "center", fill = NA)))
data_ma[, ma := c(0, ma[-c(1, .N)], 0)[1:.N], by = iso3] # first and last to zero in moving average


datan <- datam %>% subset(year_new > 2010) %>% group_by(iso3) %>%
  dplyr::summarise(Grad = (max(av_mdr_new_pcnt) - min(av_mdr_new_pcnt))/5)
ggplot(datan, aes(iso3, Grad)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


d <- datam %>%
  group_by(iso3) %>%
  nest() %>%
  mutate(model = map(data, ~lm(av_mdr_new_pcnt ~ year_new, data = .)))

xyplot(response ~ year, groups=state, data=d, type='l')

 ## when max
which.max(d_usa$av_mdr_new_pcnt)



# moving avergae
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}




d_usa <- subset(data, iso3 == "USA")
plot.ts(SMA(d_usa$av_mdr_new_pcnt, n = 8))


mav(d_usa[,c("year_new","av_mdr_new_pcnt")],10)
plot(mav(d_usa[,c("year_new","av_mdr_new_pcnt")],4))

