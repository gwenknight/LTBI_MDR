#### Plot and explore existing data on data_mdr_prev
# The proportion of new cases that are MDR-TB from multiple data sources

library(rworldmap)
library(RColorBrewer)
library(ggplot2)
theme_set(theme_bw())

####**** Aggregated data ******************************************************************************************************************************************************************************************************************************** #####

setwd("~/Documents/LTBI_MDR/datar/")
#r_orig <- read.csv("mdr_prop_all.csv") ## CHANGE ON DESKTOP
r_orig <- read.csv("who_drtb_by_country.csv")
unique(r_orig$ref) # included data sources

setwd("~/Dropbox/MRC SD Fellowship/Research/MDR/data/")

### New dataframe
r <- r_orig

####**** Map plot ******************************************************************************************************************************************************************************************************************************** #####
r2016 <-r[which(r$year == 2016),]
n2016 <- joinCountryData2Map(r2016, joinCode="NAME", nameJoinColumn="country")
mapCountryData(n2016, nameColumnToPlot="perc_new_mdr", 
               mapTitle="MDR prevalence in new, 2016 (WHO)",
               colourPalette="terrain",catMethod=c(0,0.5,1,1.5,2,3,4,5,10,15,20,25,30,35,40))
dev.copy2pdf(file="map_2016.pdf", width = 7, height = 5)

ggplot(r2016, aes(x=source_drs_year_new,y=perc_new_mdr,col=source_rr_new)) + geom_point() + 
  scale_x_continuous("Source year") + scale_color_discrete("Data source")
ggsave("WHO_2016.pdf", width = 7, height = 5)
table(r2016$source_rr_new)

rl2016 <-r[which(r$year < 2016),]
nl2016 <- joinCountryData2Map(rl2016, joinCode="NAME", nameJoinColumn="country")
mapCountryData(nl2016, nameColumnToPlot="perc_new_mdr", 
               mapTitle="MDR prevalence prior to 2016",
               colourPalette="terrain",catMethod=c(0,0.5,1,1.5,2,3,4,5,10,15,20,25,30,35,40))
dev.copy2pdf(file="map_not2016.pdf", width = 7, height = 5)

####**** Over time plots ******************************************************************************************************************************************************************************************************************************** #####
unique(r$source_drs_year_new) # NA for those without datapoints (~ 122)

## facet by country - mess
#ggplot(r, aes(x = source_drs_year_new, y = perc_new_mdr,colour = factor(ref))) + geom_point() + facet_wrap(~country, scales = "free")  + 
#  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
#ggsave("mdr_perc_all_res_bycountry.pdf")

## just WHO data
w<-which(r$ref == "cohen_2014")
ggplot(r[-w,], aes(x = source_drs_year_new, y = perc_new_mdr, label = country, colour = factor(ref))) + 
  geom_point(alpha = 0.5) + geom_text(aes(label=country),hjust = 0, vjust = 0) + 
  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("who_mdr_perc_all_refs.pdf")

ggplot(r[-w,], aes(x = source_drs_year_new, y = perc_new_mdr, ymin = perc_new_mdr_lo,  ymax = perc_new_mdr_hi,label = country, colour = factor(ref))) + 
  geom_point(alpha = 0.5) + geom_text(aes(label=country),hjust = 0, vjust = 0) + geom_errorbar() + 
  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("who_mdr_perc_all_refs_errorbars.pdf")


ggplot(r, aes(x = source_drs_year_new, y = perc_new_mdr, label = country, colour = factor(ref))) + 
  geom_point(alpha = 0.5) + geom_text(aes(label=country),hjust = 0, vjust = 0) + 
  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("mdr_perc_all_refs.pdf")

w<-which(r$source_drs_coverage_new == "Sub-national")
ggplot(r[-w,], aes(x = source_drs_year_new, y = perc_new_mdr, label = country, colour = factor(ref))) + 
  geom_point(alpha = 0.5) + geom_text(aes(label=country),hjust = 0, vjust = 0) + 
  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("mdr_perc_all_refs_no_subnat.pdf")

## example
w<-which(r$country == "Israel")
r_esto <- r[w,]  

ggplot(r_esto, aes(x = source_drs_year_new, y = perc_new_mdr, ymin = perc_new_mdr_lo,  ymax = perc_new_mdr_hi, label = country, colour = factor(ref))) + 
  geom_point(alpha = 0.5) + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + geom_errorbar() + 
  scale_y_continuous("Percentage of new cases with MDR",lim=c(0,35)) + scale_color_discrete("Reference")

## Countries with more than 5 data points
tt <- table(r$country)
cc <- names(which(tt > 4))
length(cc)
r5 <- r[match(r$country, cc, 0L)>0L, ]

ggplot(r5, aes(x = year, y = perc_new_mdr,colour = factor(ref))) + 
  geom_point() + facet_wrap(~country, scales = "free")  +
 scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("mdr_perc_all_res_bycountry_greater5.pdf", width = 15, height = 15)


#### ULTIMATELY
save(r,file='datar/mdr.Rdata')

### What percentage of high burden do these countries represent?

# countries in ARI analysis
ocn <- Reduce(intersect, list(cnz, r$iso3))
length(ocn)

