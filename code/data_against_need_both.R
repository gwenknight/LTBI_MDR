### Plot when need data against when have data

## Libraries / home
library(ggplot2); library(gdata);library(RColorBrewer); library(data.table); library(magrittr); library(dplyr); library(plyr); library(maps)
library(rworldmap)


home <- "~/Documents/LTBI_MDR/"
output <- "~/Documents/LTBI_MDR/output/"

output <- "~/Dropbox/MDR/output" # TEMPORARY

## WHO data
data <- read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1]

## plot
countries <- read.csv("~/Dropbox/MDR/107_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]

## cut up time in data
data$cut_year_new <- cut(data$year_new, seq(1969,2015,5)) # only for period when MDR around and have ARI data

store_metric <- c()
for(ii in 1:3){ #models
  
  if(ii == 1){labl = "lin"}
  if(ii == 2){labl = "quad"}
  if(ii == 3){labl = "quadc"}
  
  for(i in 1:length(countries)){
    cnn <- as.character(countries[i])
    print(cnn)
    ## Get proportion LTBI per year data
    pltbir <- read.csv(paste0("~/Dropbox/MDR/output/",cnn,"_props_ltbi_when_",labl,".csv"))[,-1]
    pltbir$country <- pltbir$cn
    
    nari <- length(unique(pltbir$mdr_rep))
    
    ## Grab upper and lower
    labs <- pltbir$yearcat
    pltbir$lowery = as.numeric( sub("\\((.+),.*", "\\1", labs) )
    pltbir$uppery = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) )
    pltbir$sum_prltbir <- round(pltbir$sum_prltbir,2)
    pltbir$sum_prltbis <- round(pltbir$sum_prltbis,2)
    pltbir$level_pr_ltbis <- cut(pltbir$sum_prltbis,seq(-0.1,1,0.1),labels=seq(0,100,10),right = TRUE)
    pltbir$level_pr_ltbir <- cut(pltbir$sum_prltbir,seq(-0.1,1,0.1),labels=seq(0,100,10),right = TRUE)
    
    p <-subset(pltbir, lowery > 1968)
    d <-data[which(data$iso3 == cnn),]
    d$type <- "MDR"
    
    ggplot() + 
      geom_rect( data = p[which(pltbir$mdr_rep < 9),] , 
                 mapping = aes( xmin = lowery , 
                                xmax = uppery ,
                                ymin = 0 ,
                                ymax = 50 ,
                                fill = level_pr_ltbir) ,
                 alpha = .5 ) + facet_wrap(~mdr_rep, ncol = 4) + 
      scale_fill_brewer(name = "Level of\ncontribution\nto MDR LTBI",drop = FALSE,direction = -1,palette = "Spectral", 
                        labels = c("0","(0-10%]","(10-20%]","(20-30%]","(30-40%]","(40-50%]","(50-60%]",
                                   "(60-70%]","(70-80%]","(80-90%]","(90-100%]")) + 
      scale_x_continuous("Year") + scale_y_continuous("Percentage of new cases that are MDR") + 
      geom_point( data = d , aes(x=year_new, y = 100*new_mdr_prop, group = iso3)) 
    ggsave(paste0("~/Dropbox/MDR/output/eg_when_imp_vs_data_",cnn,"_",nari,"_",labl,".pdf"),width = 12, height = 4)
    
    ### Mean over mdr_rep
    mean_pr <- pltbir %>% group_by(yearcat,lowery,uppery) %>%
      dplyr::summarise(mean=mean(sum_prltbir), min=quantile(sum_prltbir, 0.025), max=quantile(sum_prltbir, 0.975))
    
    mean_ps <- pltbir %>% group_by(yearcat,lowery,uppery) %>%
      dplyr::summarise(mean=mean(sum_prltbis), min=quantile(sum_prltbis, 0.025), max=quantile(sum_prltbis, 0.975))
    
    mean_ps$type <- "DS"
    mean_pr$type <- "MDR"
    mean_both <- rbind(mean_ps,mean_pr)
    
    mean_both$level_pr_ltbi <- cut(mean_both$mean,seq(-0.1,1,0.1),labels=seq(0,100,10),right = TRUE)
    
    ggplot() + 
      geom_rect( data = mean_both , 
                 mapping = aes( xmin = lowery , 
                                xmax = uppery ,
                                ymin = 0 ,
                                ymax = 50 ,
                                fill = level_pr_ltbi) ,
                 alpha = .5 ) + facet_wrap(~type, ncol = 2) + 
      scale_fill_brewer(name = "Level of\ncontribution\nto MDR LTBI",drop = FALSE,direction = -1,palette = "Spectral", 
                        labels = c("0","(0-10%]","(10-20%]","(20-30%]","(30-40%]","(40-50%]","(50-60%]",
                                   "(60-70%]","(70-80%]","(80-90%]","(90-100%]")) + 
      scale_x_continuous("Year") + scale_y_continuous("Percentage of new cases that are MDR") + 
      geom_point( data = d , aes(x=year_new, y = 100*new_mdr_prop, group = iso3)) + 
      geom_errorbar(data = d, aes(x= year_new, ymax = 100*mhi, ymin = 100*mlo))
    ggsave(paste0("~/Dropbox/MDR/output/when_imp_vs_data_mean_",cnn,"_",nari,"_",labl,".pdf"),width = 12, height = 4)
    
    
    ## METRIC
    ## sum over 5 yrs % LTBI from that 5 yr x data there (y/n) = M. High = good low = ba
    ## table of those with data in that year group (0 if none, 1 if some) * proportion of ltbi in there 
    p$metric <-   rep(ifelse(table(d$cut_year_new)>0,1,0),length(unique(p$mdr_rep))) * as.numeric(p$sum_prltbir) 
    p <- as.data.table(p)
    ## Sum Metric = proportion of LTBI that could be gotten at using this data
    pp <- ddply(p, .(mdr_rep), summarize,  sum_metric=sum(metric)) #pp <- p %>% group_by(mdr_rep) %>% summarise(sum_metric = sum(metric))
    store_metric <- rbind(store_metric,cbind(pp,cnn,ii))
  }
}

## UP TO HERE
store_metric <- as.data.frame(store_metric)
colnames(store_metric) <- c("mdr_rep","sum_metric","cnn","model")
write.csv(store_metric, paste0("~/Dropbox/MDR/output/store_metric_",nari,".csv"))

store_metric <- read.csv(paste0("~/Dropbox/MDR/output/store_metric_",nari,".csv"))[,-1]

if(ii == 1){labl = "lin"}
if(ii == 2){labl = "quad"}
if(ii == 3){labl = "quadc"}

for(ii in 1:3){
  ggplot(store_metric[which(store_metric$model == ii),], aes(x=mdr_rep, y = sum_metric)) + geom_point() + 
    facet_wrap(~cnn, ncol = 18) + scale_y_continuous("Proportion of LTBI identifiable") + 
    scale_x_continuous("MDR-ARI trend")
  ggsave(paste0("~/Dropbox/MDR/output/MDR_metric_data_against_need_by_country_",nari,"_",labl,".pdf"), width = 14, height = 14)
  
  ggplot(store_metric[which(store_metric$model == ii),], aes(x=cnn, y = sum_metric)) + geom_boxplot() + 
    scale_y_continuous("Proportion of LTBI identifiable") + 
    scale_x_discrete("Country")
  ggsave(paste0("~/Dropbox/MDR/output/MDR_metric_data_against_need_",nari,"_",labl,".pdf"), width = 14, height = 14)

  
  #### *** MAP **** ###
  
  store_metric_med <- as.data.frame(store_metric) %>% group_by(cnn, model) %>% dplyr::summarise(med = median(sum_metric))
  
  # define color buckets
  store_metric_med$metric <- as.numeric(cut(store_metric_med$med, c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7, 0.8, 0.9, 1)))
  store_metric_med <- as.data.frame(store_metric_med)
  
  mapped_data <- joinCountryData2Map(store_metric_med, joinCode = "ISO3", nameJoinColumn = "cnn")
  
  pdf(paste0("../output/map_metric_",nari,"_",labl,".pdf"))
  mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "med", catMethod = seq(0,1,0.1),
                              colourPalette = cols,
                              addLegend = FALSE)
  do.call( addMapLegend, c( mapParams
                            , legendLabels="all"
                            , legendWidth=0.5
                            
  ))
  dev.off()
}
