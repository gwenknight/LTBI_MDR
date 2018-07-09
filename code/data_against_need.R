### Plot when need data against when have data

## Libraries / home
library(ggplot2); library(gdata);library(RColorBrewer); library(data.table); library(magrittr); library(dplyr)

home <- "~/Documents/LTBI_MDR/"
output <- "~/Documents/LTBI_MDR/output/"

## Get WHO data
setwd(home)
data <- read.csv("datar/new_who_edited.csv")

## Get proportion LTBI per year data
pltbir <- read.csv("output/props_ltbi_when.csv")[,-1]
pltbir$country <- pltbir$cn

## Grab upper and lower
labs <- pltbir$yearcat
pltbir$lowery = as.numeric( sub("\\((.+),.*", "\\1", labs) )
pltbir$uppery = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) )
pltbir$sum_prltbi <- round(pltbir$sum_prltbi,2)
pltbir$level_pr_ltbir <- cut(pltbir$sum_prltbi,seq(0,1,0.1),labels=seq(1,10,1),right = FALSE)

## plot
countries <- unique(pltbir$country)
setwd(output)

## cut up time in data
data$cut_year_new <- cut(data$year_new, seq(1969,2015,5)) # only for period when MDR around and have ARI data

store_metric <- c()

for(i in 1:4){
  cnn <- as.character(countries[i])
  p <-subset(pltbir, country == cnn & lowery > 1968)
  d <-subset(data, country == cnn )
  
  ggplot() + 
    geom_rect( data = p , 
               mapping = aes( xmin = lowery , 
                              xmax = uppery ,
                              ymin = 0 ,
                              ymax = 50 ,
                              fill = level_pr_ltbir) ,
               alpha = .5 ) + facet_wrap(~mdr_rep) + 
    scale_fill_brewer(name = "Level of\ncontribution\nto LTBI",drop = FALSE,direction = -1,palette = "Spectral") + 
    scale_x_continuous("Year") + scale_y_continuous("Percentage of new cases that are MDR") + 
    geom_point( data = d , aes(x=year_new, y = mdr_new_pcnt, group = country))
  ggsave(paste0("when_imp_vs_data_",cnn,".pdf"))
  
  ## METRIC
  ## sum over 5 yrs % LTBI from that 5 yr x data there (y/n) = M. High = good low = ba
  ## table of those with data in that year group (0 if none, 1 if some) * proportion of ltbi in there 
  p$metric <-   rep(ifelse(table(d$cut_year_new)>0,1,0),length(unique(p$mdr_rep))) * as.numeric(p$sum_prltbi) 
  p <- as.data.table(p)
  ## Sum Metric = proportion of LTBI that could be gotten at using this data
  pp <- p %>% group_by(mdr_rep) %>% summarise(sum_metric = sum(metric))
  store_metric <- rbind(store_metric,cbind(pp,cnn))
}

store_metric <- as.data.frame(store_metric)
colnames(store_metric) <- c("mdr_rep","sum_metric","cnn")

ggplot(store_metric, aes(x=mdr_rep, y = sum_metric)) + geom_point() + 
  facet_wrap(~cnn) + scale_y_continuous("Proportion of LTBI identifiable") + 
  scale_x_continuous("MDR-ARI trend")
ggsave("MDR_metric_data_against_need.pdf")








