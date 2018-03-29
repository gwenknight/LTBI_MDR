#### Countries plot

library(rworldmap)
library(RColorBrewer)

###### mdr-tb proportions
r <- read.csv("mdr_prop_all.csv")

n <- joinCountryData2Map(r, joinCode="NAME", nameJoinColumn="country")

# ALL
mapCountryData(n, nameColumnToPlot="perc_new_mdr", mapTitle="World",
               colourPalette="terrain")


## years
unique(r$source_drs_year_new)
ggplot(r, aes(x = source_drs_year_new, y = perc_new_mdr,colour = factor(ref))) + geom_point() + 
  facet_wrap(~country, scales = "free")  + 
  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("mdr_perc_all_res_bycountry.pdf")

ggplot(r, aes(x = source_drs_year_new, y = perc_new_mdr, label = country, colour = factor(ref))) + 
  geom_point(alpha = 0.5) + geom_text(aes(label=country),hjust = 0, vjust = 0) + 
  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")
ggsave("mdr_perc_all_refs.pdf")

## example
w<-which(r$country == "Estonia")
r_esto <- r[w,]  

ggplot(r_esto, aes(x = source_drs_year_new, y = perc_new_mdr, label = country, colour = factor(ref))) + 
  geom_point(alpha = 0.5) + geom_text(aes(label=country),hjust = -0.1, vjust = 0) + 
  scale_y_continuous("Percentage of new cases with MDR") + scale_color_discrete("Reference")

