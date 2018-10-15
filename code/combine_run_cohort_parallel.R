#### Run combine output from run_cohort_ltbi_mdr 

###********** Libraries and ggplot theme ************************************************************************************************#######
library(ggplot2)
theme_set(theme_bw(base_size = 24))
library(plyr)
library(dplyr)
library(cowplot)
library(data.table)
library(reshape2)

###********** Home ************************************************************************************************#######

###********** Load code and data ************************************************************************************************#######
setwd("~/Documents/LTBI_MDR/code")
## Population size 2014
load('../data/POP2014.Rdata')  

###********** Run for different countries ************************************************************************************************************************#######

## WHO data
w_data <- read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1]

## Which countries? 
cni <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
length(cni) # 138
llu <- length(cni)

nari <- 200

# DS and MDR data

# Label for plots 
labl <- "infor_prior"

# Store all? 
store_all <- as.data.frame(matrix(0,length(cni)*4*81*100,9))
runn <- 1
level2014 <- c(); #breakdown proportions infected by age
s_level <- c(); #sum proportions infected 

# Run for this country

for(i in 1:llu){
  cnn <- cni[i]
  print(c(i,cnn))  
  ### READ IN and BUILD
  level2014 <- rbind(level2014,read.csv(paste0("~/Dropbox/MDR/output/",cnn,"level2014_",nari,"_",labl,".csv"),stringsAsFactors = FALSE)[,-1])
  s_level <- rbind(s_level,read.csv(paste0("~/Dropbox/MDR/output/",cnn,"s_level_",nari,"_",labl,".csv"),stringsAsFactors = FALSE)[,-1])
  
}
write.csv(s_level, paste0("~/Dropbox/MDR/output/s_level_",nari,"_",labl,".csv"))
write.csv(level2014, paste0("~/Dropbox/MDR/output/level2014_",nari,"_",labl,".csv"))

###********** PLOTS *****************************************************************#####
a2r<-ggplot(s_level, aes(x=pop_name, y = ltbir, col=factor(rep) )) + geom_point() + 
  guides(colour=FALSE) + 
  scale_x_discrete("Country") + scale_y_continuous("LTBI-MDR\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbir),y=ltbir) 
a2r
save_plot(paste0("~/Dropbox/MDR/output/ltbi_all_countries_r",nari,"_",labl,".pdf"), a2r, base_aspect_ratio = 1.5 )

a2s<-ggplot(s_level, aes(x=pop_name, y = ltbis, col=factor(rep) )) + geom_point() + 
  guides(colour=FALSE) + 
  scale_x_discrete("Country") + scale_y_continuous("LTBI-DS\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbis),y=ltbis) 
a2s
save_plot(paste0("~/Dropbox/MDR/output/ltbi_all_countries_s",nari,"_",labl,".pdf"), a2s, base_aspect_ratio = 1.5 )

s_level_median <- s_level %>%
  group_by(pop_name) %>% 
  summarise_at(c("ltbir","ltbis","pltbir","pltbis"),funs(median)) 

ub <- function(x)quantile(x,probs = .975)
lb <- function(x)quantile(x,probs = .025)

s_level_lo <- s_level %>%
  group_by(pop_name) %>% 
  summarise_at(c("ltbir","ltbis","pltbir","pltbis"),funs(lb))
colnames(s_level_lo) <- c("pop_name","ltbir.lo","ltbis.lo","pltbir.lo","pltbis.lo")

s_level_hi <- s_level %>%
  group_by(pop_name) %>% 
  summarise_at(c("ltbir","ltbis","pltbir","pltbis"),funs(ub))
colnames(s_level_hi) <- c("pop_name","ltbir.hi","ltbis.hi","pltbir.hi","pltbis.hi")

s_level_median <- merge(s_level_median, s_level_lo, by = "pop_name")
s_level_median <- merge(s_level_median, s_level_hi, by = "pop_name")


a2r<-ggplot(s_level_median, aes(x=pop_name, y = ltbir )) + geom_point() + 
  scale_x_discrete("") + scale_y_continuous("") + #LTBI-MDR\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbir),y=ltbir) +
  geom_errorbar(aes(min = ltbir.lo, max = ltbir.hi))
a2r
save_plot(paste0("~/Dropbox/MDR/output/ltbi_all_countries_r_mean",nari,"_",labl,".pdf"), a2r, base_aspect_ratio = 0.8)

a2s<-ggplot(s_level_median, aes(x=pop_name, y = ltbis )) + geom_point() +  
  scale_x_discrete("Country") + scale_y_continuous("LTBI-DS\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbis),y=ltbis)  +
  geom_errorbar(aes(min = ltbis.lo, max = ltbis.hi))
a2s
save_plot(paste0("~/Dropbox/MDR/output/ltbi_all_countries_s_mean",nari,"_",labl,".pdf"), a2s, base_aspect_ratio = 1.5 )

w<-which(s_level_median$pltbir > 324)
a2r<-ggplot(s_level_median[w,], aes(x=pop_name, y = pltbir)) + geom_point(aes(col=ltbir)) + 
  scale_y_continuous("Number with MDR-LTBI (thousands)") + #LTBI-MDR\n(% population infected)") +
  theme(axis.text.y = element_text(size = 12)) + 
  coord_flip() + aes(x=reorder(pop_name,pltbir),y=pltbir) +
  geom_linerange(aes(min = pltbir.lo, max = pltbir.hi,col=ltbir)) + 
  scale_x_discrete("Country iso3 code") + scale_color_continuous("LTBIR\n(%)")
a2r
ggsave(paste0("~/Dropbox/MDR/output/ltbi_all_countries_numb_r_mean",nari,"_",labl,".pdf"), width = 10, height = 13)

#### *** MAP **** ###
library(maps)
# define color buckets
cols = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043","grey")
#s_level_median$ltbis_c <- as.numeric(cut(s_level_median$ltbis, c(0, 10, 20, 30, 40, 50,100)))
#s_level_median$ltbis_r <- as.numeric(cut(s_level_median$ltbis, c(0, 1, 2, 3, 4, 5, 6)))

library(ggplot2)
library(rworldmap)
library(RColorBrewer)


mapped_data <- joinCountryData2Map(s_level_median, joinCode = "ISO3", nameJoinColumn = "pop_name")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
cols <- colorRampPalette(brewer.pal(11,"Reds"), bias = 2)(13)

pdf(paste0("~/Dropbox/MDR/output/map_ltbis_",nari,"_",labl,".pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbis", catMethod = seq(0,50,10),
                            colourPalette = cols,
                            addLegend = FALSE)
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5
                          
))
dev.off()

pdf(paste0("~/Dropbox/MDR/output/map_ltbir_",nari,"_",labl,".pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbir", catMethod = seq(0,4,0.25),
                            colourPalette = cols,
                            addLegend = FALSE)
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5
                          
))
dev.off()


