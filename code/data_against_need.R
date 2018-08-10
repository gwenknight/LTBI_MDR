### Plot when need data against when have data

## Libraries / home
library(ggplot2); library(gdata);library(RColorBrewer); library(data.table); library(magrittr); library(dplyr)

home <- "~/Documents/LTBI_MDR/"
output <- "~/Documents/LTBI_MDR/output/"

output <- "~/Dropbox/MDR/output" # TEMPORARY

## Get WHO data
setwd(home)
data <- read.csv("datar/new_who_edited.csv")

## Get proportion LTBI per year data
setwd(output)
pltbir <- read.csv("props_ltbi_when.csv")[,-1]
pltbir$country <- pltbir$cn

## Grab upper and lower
labs <- pltbir$yearcat
pltbir$lowery = as.numeric( sub("\\((.+),.*", "\\1", labs) )
pltbir$uppery = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) )
pltbir$sum_prltbi <- round(pltbir$sum_prltbi,2)
pltbir$level_pr_ltbir <- cut(pltbir$sum_prltbi,seq(-0.1,1,0.1),labels=seq(0,100,10),right = TRUE)

## plot
countries <- unique(pltbir$country)
setwd(output)

## cut up time in data
data$cut_year_new <- cut(data$year_new, seq(1969,2015,5)) # only for period when MDR around and have ARI data

store_metric <- c()

for(i in 1:length(countries)){
  cnn <- as.character(countries[i])
  p <-subset(pltbir, country == cnn & lowery > 1968)
  d <-data[which(data$iso3 == cnn),]
  
  ggplot() + 
    geom_rect( data = p , 
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
    geom_point( data = d , aes(x=year_new, y = mdr_new_pcnt, group = country))
  ggsave(paste0("when_imp_vs_data_",cnn,".pdf"),width = 12, height = 4)
  
  ## METRIC
  ## sum over 5 yrs % LTBI from that 5 yr x data there (y/n) = M. High = good low = ba
  ## table of those with data in that year group (0 if none, 1 if some) * proportion of ltbi in there 
  p$metric <-   rep(ifelse(table(d$cut_year_new)>0,1,0),length(unique(p$mdr_rep))) * as.numeric(p$sum_prltbi) 
  p <- as.data.table(p)
  ## Sum Metric = proportion of LTBI that could be gotten at using this data
  pp <- ddply(p, .(mdr_rep), summarize,  sum_metric=sum(metric)) #pp <- p %>% group_by(mdr_rep) %>% summarise(sum_metric = sum(metric))
  store_metric <- rbind(store_metric,cbind(pp,cnn))
}

store_metric <- as.data.frame(store_metric)
colnames(store_metric) <- c("mdr_rep","sum_metric","cnn")

ggplot(store_metric, aes(x=mdr_rep, y = sum_metric)) + geom_point() + 
  facet_wrap(~cnn, ncol = 18) + scale_y_continuous("Proportion of LTBI identifiable") + 
  scale_x_continuous("MDR-ARI trend")
ggsave("MDR_metric_data_against_need.pdf", width = 14, height = 14)

ggplot(store_metric, aes(x=cnn, y = sum_metric)) + geom_point(aes(col = sum_metric)) + 
 scale_y_continuous("Proportion of LTBI identifiable") + 
  scale_x_discrete("Country") + facet_wrap(~mdr_rep)
ggsave("MDR_metric_data_against_need_bycn.pdf")

ggplot(store_metric, aes(x=cnn, y = sum_metric)) + geom_point(aes(col = factor(mdr_rep))) + geom_line(aes(group = cnn)) + 
  scale_y_continuous("Proportion of LTBI identifiable") + 
  scale_x_discrete("Country") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("MDR_metric_data_against_need_bytype.pdf")

### Get ltbi levels
s_level <- read.csv("s_level_7.csv")[,-1]
s_level$cnn <- s_level$pop_name
s_level$mdr_rep <- s_level$rep
totals <- merge(store_metric, s_level, by = c("cnn", "mdr_rep"))

ggplot(subset(totals, best == 1), aes(x=cnn, y = sum_metric)) + geom_point(aes(col = factor(mdr_rep))) + 
  geom_line(aes(group = cnn)) + 
  scale_y_continuous("Proportion of LTBI identifiable") + scale_color_discrete("MDR\nTrend") +
  scale_x_discrete("Country") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("MDR_metric_data_against_need_total_best.pdf", width = 14, height = 8)

ggplot(subset(totals, best == 1), aes(x=cnn, y = ltbir)) + geom_point(aes(col = factor(mdr_rep))) + geom_line(aes(group = cnn)) + 
  scale_y_continuous("Percentage with LTBI-MDR")+ scale_color_discrete("MDR\nTrend")  + 
  scale_x_discrete("Country") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("LTBIR_total_best.pdf", width = 14, height = 6)

ggplot(subset(totals, best == 1), aes(x=cnn, y = ltbis)) + geom_point(aes(col = factor(mdr_rep))) + geom_line(aes(group = cnn)) + 
  scale_y_continuous("Percentage with LTBI-DS")+ scale_color_discrete("MDR\nTrend")  + 
  scale_x_discrete("Country") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("LTBIS_total_best.pdf", width = 14, height = 6)

# Case studies
cs1 <- "BRA"
w<-which(totals$cnn == cs1)
mm <- melt(totals[w,c("mdr_rep", "sum_metric","ltbir","ltbis","best")],id.vars=c("mdr_rep","best")) 
ggplot(mm,aes(x = mdr_rep, y = value)) + geom_point(aes(pch = factor(best))) + 
  ggtitle(cs1) + facet_wrap(~variable, scales = "free") + scale_shape_discrete("Best\nMDR\nType",labels = c("Best just type","Best all types"))
ggsave(paste0("CS",cs1,"_final.pdf"), width = 14, height = 6)

cs2 <- "UKR"
w<-which(totals$cnn == cs1)
mm <- melt(totals[w,c("mdr_rep", "sum_metric","ltbir","ltbis","best")],id.vars=c("mdr_rep","best")) 
ggplot(mm,aes(x = mdr_rep, y = value)) + geom_point(aes(pch = factor(best))) + 
  ggtitle(cs2) + facet_wrap(~variable, scales = "free") + scale_shape_discrete("Best\nMDR\nType",labels = c("Best just type","Best all types"))
ggsave(paste0("CS",cs2,"_final.pdf"), width = 14, height = 6)


# stats
ff <- subset(totals, best == 1)
100*length(which(ff$ltbir > 1)) / length(ff$ltbir) # 10% > 1%
mean(ff$ltbir) # mean is 0.3% have ltbir

####**** Map plot ******************************************************************************************************************************************************************************************************************************** #####
s_levelb <-s_level[,] # slice by mdr_rep?


n2016 <- joinCountryData2Map(r2016, joinCode="NAME", nameJoinColumn="country")
mapCountryData(n2016, nameColumnToPlot="ltbir", 
               mapTitle="MDR LTBI percentage of population, 2014",
               colourPalette="terrain",catMethod=c(0,0.5,1,1.5,2,3,4,5,10,15,20,25,30,35,40))
dev.copy2pdf(file="MDRLTBI_map_2014.pdf", width = 7, height = 5)
