#### Plot and explore existing data on data_mdr_prev
# The proportion of new cases that are MDR-TB from multiple data sources

library(rworldmap)
library(RColorBrewer)
library(ggplot2)

####**** Aggregated data ******************************************************************************************************************************************************************************************************************************** #####

setwd("~/Documents/LTBI_MDR/data/")
r_orig <- read.csv("mdr_prop_all.csv")
unique(r_orig$ref) # included data sources

### New dataframe
r <- r_orig

####**** Map plot ******************************************************************************************************************************************************************************************************************************** #####
#n <- joinCountryData2Map(r, joinCode="NAME", nameJoinColumn="country")
#mapCountryData(n, nameColumnToPlot="perc_new_mdr", mapTitle="World",colourPalette="terrain")

####**** Over time plots ******************************************************************************************************************************************************************************************************************************** #####
setwd("../output")

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
cc <- names(which(tt >= 4))
length(cc)
r5 <- r[match(r$country, cc, 0L)>0L, ]

ggplot(r5, aes(x = source_drs_year_new, y = perc_new_mdr,colour = factor(ref))) + geom_point() + facet_wrap(~country)  +
 scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("mdr_perc_all_res_bycountry_greater5.pdf")


#### ULTIMATELY
save(r,file='datar/mdr.Rdata')



# countries in ARI analysis
ocn <- Reduce(intersect, list(cnz, r$iso3))
length(ocn)

