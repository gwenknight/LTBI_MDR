### Plot when need data against when have data

## Libraries / home
library(ggplot2); library(gdata)

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
    scale_fill_brewer(name = "Level of\ncontribution\nto LTBI") + 
    scale_x_continuous("Year") + scale_y_continuous("Percentage of new cases that are MDR") + 
    geom_point( data = d , aes(x=year_new, y = mdr_new_pcnt, group = country))
  ggsave(paste0("when_imp_vs_data_",cnn,".pdf"))
  
  ## METRIC
  # sum over 5 yrs % LTBI from that 5 yr x data there (y/n) = M. High = good low = bad
  #d$cut_year_new <- cut(d$year_new, seq(1929,2020,5))
  
  
}









