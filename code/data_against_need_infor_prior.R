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
countries <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]

## cut up time in data
data$cut_year_new <- cut(data$year_new, seq(1969,2015,5)) # only for period when MDR around and have ARI data

store_metric <- c()
labl = "infor_prior"

big_time <- c()

for(i in 1:length(countries)){
  cnn <- as.character(countries[i])
  print(cnn)
  ## Get proportion LTBI per year data
  pltbir <- read.csv(paste0("~/Dropbox/MDR/output/",cnn,"_props_ltbi_when_",labl,".csv"))[,-1]
  pltbir$country <- pltbir$cn
  
  nari <- length(unique(pltbir$mdr_rep))
  
  # Grab biggest contributing time
  w<-which.max(pltbir$sum_prltbir)
  big_time <- c(big_time,pltbir[w,"yearcat"])
  
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
    geom_rect( data = p[which(p$mdr_rep < 9),] ,
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
    geom_point( data = d , aes(x=year_new, y = 100*new_mdr_prop, group = iso3)) +
    geom_errorbar(data = d, aes(x= year_new, ymax = 100*mhi, ymin = 100*mlo))
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
  p$metric <-   rep(ifelse(table(d$cut_year_new)>0,1,0),each = length(unique(p$mdr_rep))) * as.numeric(p$sum_prltbir)
  p <- as.data.table(p)
  ## Sum Metric = proportion of LTBI that could be gotten at using this data
  pp <- ddply(p, .(mdr_rep), summarize,  sum_metric=sum(metric)) #pp <- p %>% group_by(mdr_rep) %>% summarise(sum_metric = sum(metric))
  store_metric <- rbind(store_metric,cbind(pp,cnn,i))
}


## PLOT
store_metric <- as.data.frame(store_metric)
colnames(store_metric) <- c("mdr_rep","sum_metric","cnn","model")
write.csv(store_metric, paste0("~/Dropbox/MDR/output/store_metric_",nari,".csv"))

######*****#######
######*****#######
######*****#######
######*****#######
# READ in if need
store_metric <- read.csv(paste0("~/Dropbox/MDR/output/store_metric_",nari,".csv"))[,-1]

ggplot(store_metric, aes(x=mdr_rep, y = sum_metric)) + geom_point() + 
  facet_wrap(~cnn, ncol = 18) + scale_y_continuous("Metric for MDR-LTBI data coverage") + 
  scale_x_continuous("MDR-ARI trend") + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
ggsave(paste0("~/Dropbox/MDR/output/MDR_metric_data_against_need_by_country_",nari,"_",labl,".pdf"), width = 14, height = 14)

ggplot(store_metric, aes(x=cnn, y = sum_metric )) + geom_boxplot() + 
  scale_x_discrete("Country") + scale_y_continuous("Proportion of LTBI identifiable") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(cnn,sum_metric),y=sum_metric) 
ggsave(paste0("~/Dropbox/MDR/output/MDR_metric_data_against_need_",nari,"_",labl,".pdf"), width = 14, height = 14)

## Top 30 HBC MDR
mdr30 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/top30_mdr_countries.csv")

# add in mean
store_metric$mean_s <- 0
store_median <- c()
for(i in 1:138){w<-which(as.character(store_metric$cnn) == as.character(countries[i]));
mean_s <- median(store_metric[w,"sum_metric"]);
store_median <- c(store_median,mean_s);
store_metric[w,"mean_s"] <- mean_s}

# medians for each country
store_median <- cbind(store_median, countries)
w<-c(which(store_median[,2] == "TGO"),which(store_median[,2] == "DJI"),
     which(store_median[,2] == "SDN"),which(store_median[,2] == "BTN"))
s <- store_median[-w,]
median(as.numeric(s[,1]))
range(as.numeric(s[,1]))

# order
oo <- order(as.numeric(store_median[,1]), decreasing = TRUE)
store_median[oo,]

## mdr30 
w<-c()
for(i in 1:30){w<-c(w,which(as.character(store_metric$cnn) == as.character(mdr30[i,"iso3"])))}
theme_set(theme_bw())

ggplot(store_metric[w,], aes(x=cnn, y = sum_metric )) + geom_boxplot(outlier.shape = NA) + 
  scale_x_discrete("Country") + scale_y_continuous("Metric for MDR-LTBI data coverage") +
  theme(axis.text.y = element_text(size = 10)) + 
  coord_flip() + aes(x=reorder(cnn,mean_s),y=sum_metric) 
ggsave(paste0("~/Dropbox/MDR/output/MDR_metric_data_against_need_",nari,"_",labl,"_top30.pdf"), width = 14, height = 14)

store_metric[w,]

## Single country
w<- which(store_metric$cnn == "BWA")
median(store_metric[w,"sum_metric"])
ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)
ub(store_metric[w,"sum_metric"])
lb(store_metric[w,"sum_metric"])

paste0(prettyNum(signif(median(store_metric[w,"sum_metric"]),3),big.mark=","), " [", 
       prettyNum(signif(lb(store_metric[w,"sum_metric"]),3),big.mark=","), "-", 
       prettyNum(signif(ub(store_metric[w,"sum_metric"]),4),big.mark=","),"]")


#### *** MAP **** ###

store_metric_med <- as.data.frame(store_metric) %>% group_by(cnn, model) %>% dplyr::summarise(med = median(sum_metric))

# define color buckets
store_metric_med$metric <- as.numeric(cut(store_metric_med$med, c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7, 0.8, 0.9, 1)))
store_metric_med <- as.data.frame(store_metric_med)
cols <- colorRampPalette(brewer.pal(11,"Reds"), bias = 2)(13)

mapped_data <- joinCountryData2Map(store_metric_med, joinCode = "ISO3", nameJoinColumn = "cnn")

pdf(paste0("~/Dropbox/MDR/output/map_metric_",nari,"_",labl,".pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "med", catMethod = seq(0,1,0.1),
                            colourPalette = cols,
                            addLegend = FALSE,missingCountryCol = gray(.8))
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5
                          
))
dev.off()


#### Run output production to get med.ltbir
store_metric$iso3 <- store_metric$cnn

mm <- merge(store_metric, med.ltbir[,c("iso3","ltbir")], by = "iso3")

ggplot(mm, aes(x=cnn, y = sum_metric )) + geom_boxplot() + 
  scale_x_discrete("Country") + scale_y_continuous("Proportion of LTBI identifiable") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(cnn,ltbir),y=sum_metric) 
ggsave(paste0("~/Dropbox/MDR/output/MDR_metric_data_against_need_",nari,"_",labl,"order_by_ltbir.pdf"), width = 14, height = 14)

## Top 30 HBC MDR to rank
w<-c()
for(i in 1:30){w<-c(w,which(as.character(mm$cnn) == as.character(mdr30[i,"iso3"])))}

ggplot(mm[w,], aes(x=cnn, y = sum_metric )) + geom_boxplot(outlier.shape = NA) + 
  scale_x_discrete("Country") + scale_y_continuous("Proportion of LTBI identifiable") +
  theme(axis.text.y = element_text(size = 10)) + 
  coord_flip() + aes(x=reorder(cnn,ltbir),y=sum_metric) 
ggsave(paste0("~/Dropbox/MDR/output/MDR_metric_data_against_need_",nari,"_",labl,"order_by_ltbir_top30.pdf"), width = 14, height = 14)
