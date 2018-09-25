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
## Population size 2014
load('../data/POP2014.Rdata')  

###********** Run for different countries ************************************************************************************************************************#######

## WHO data
w_data <- read.csv("../data/new_who_edited_sub.csv")[,-1]

## Which countries? 
cni <- read.csv("../data/107_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
length(cni) # 107
llu <- length(cni)

nari <- 1000

# DS and MDR data
for(ii in 1:3){ # 3 models
  
  # Label for plots 
  if(ii == 1){pp <- "lin"}
  if(ii == 2){pp <- "quadc"}
  if(ii == 3){pp <- "quad"}
  
  # Store all? 
  store_all <- as.data.frame(matrix(0,length(cni)*4*81*100,9))
  runn <- 1
  level2014 <- c(); #breakdown proportions infected by age
  s_level <- c(); #sum proportions infected 
  
  print(ii)
  
  # Run for this country
  
  for(i in 1:llu){
    cnn <- cni[i]
    
    ### READ IN and BUILD
    level2014 <- rbind(level2014,read.csv(paste0("../output/",cnn,"_level2014_",nari,"_",pp,".csv"),stringsAsFactors = FALSE)[,-1])
    s_level <- rbind(s_level,read.csv(paste0("../output/",cnn,"_s_level_",nari,"_",pp,".csv"),stringsAsFactors = FALSE)[,-1])
    
  }
  write.csv(s_level, paste0("../output/s_level_",nari,"_",labl,".csv"))
  write.csv(level2014, paste0("../output/level2014_",nari,"_",labl,".csv"))
  
  ###********** PLOTS *****************************************************************#####
  a2r<-ggplot(s_level, aes(x=pop_name, y = ltbir, col=factor(rep) )) + geom_point() + 
    guides(colour=FALSE) + 
    scale_x_discrete("Country") + scale_y_continuous("LTBI-MDR\n(% population infected)") +
    theme(axis.text.y = element_text(size = 6)) + 
    coord_flip() + aes(x=reorder(pop_name,ltbir),y=ltbir) 
  a2r
  save_plot(paste0("../output/ltbi_all_countries_r",nari,"_",pp,".pdf"), a2r, base_aspect_ratio = 1.5 )
  
  a2s<-ggplot(s_level, aes(x=pop_name, y = ltbis, col=factor(rep) )) + geom_point() + 
    guides(colour=FALSE) + 
    scale_x_discrete("Country") + scale_y_continuous("LTBI-DS\n(% population infected)") +
    theme(axis.text.y = element_text(size = 6)) + 
    coord_flip() + aes(x=reorder(pop_name,ltbis),y=ltbis) 
  a2s
  save_plot(paste0("../output/ltbi_all_countries_s",nari,"_",pp,".pdf"), a2s, base_aspect_ratio = 1.5 )
  
  s_level_mean <- s_level %>%
    group_by(pop_name) %>% 
    summarise_at(c("ltbir","ltbis","pltbir","pltbis"),funs(mean)) 
  
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
  
  s_level_mean <- merge(s_level_mean, s_level_lo, by = "pop_name")
  s_level_mean <- merge(s_level_mean, s_level_hi, by = "pop_name")
  
  
  a2r<-ggplot(s_level_mean, aes(x=pop_name, y = ltbir )) + geom_point() + 
    scale_x_discrete("") + scale_y_continuous("") + #LTBI-MDR\n(% population infected)") +
    theme(axis.text.y = element_text(size = 6)) + 
    coord_flip() + aes(x=reorder(pop_name,ltbir),y=ltbir) +
    geom_errorbar(aes(min = ltbir.lo, max = ltbir.hi))
  a2r
  save_plot(paste0("../output/ltbi_all_countries_r_mean",nari,"_",pp,".pdf"), a2r, base_aspect_ratio = 0.8)
  
  a2s<-ggplot(s_level_mean, aes(x=pop_name, y = ltbis )) + geom_point() +  
    scale_x_discrete("Country") + scale_y_continuous("LTBI-DS\n(% population infected)") +
    theme(axis.text.y = element_text(size = 6)) + 
    coord_flip() + aes(x=reorder(pop_name,ltbis),y=ltbis)  +
    geom_errorbar(aes(min = ltbis.lo, max = ltbis.hi))
  a2s
  save_plot(paste0("../output/ltbi_all_countries_s_mean",nari,"_",pp,".pdf"), a2s, base_aspect_ratio = 1.5 )
  
  #### *** MAP **** ###
  library(maps)
  # define color buckets
  cols = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043","grey")
  s_level_mean$ltbis_c <- as.numeric(cut(s_level_mean$ltbis, c(0, 10, 20, 30, 40, 50,100)))
  s_level_mean$ltbis_r <- as.numeric(cut(s_level_mean$ltbis, c(0, 1, 2, 3, 4, 5, 6)))
  
  library(ggplot2)
  library(rworldmap)
  library(RColorBrewer)
  
  
  mapped_data <- joinCountryData2Map(s_level_mean, joinCode = "ISO3", nameJoinColumn = "pop_name")
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
  cols <- colorRampPalette(brewer.pal(11,"Reds"), bias = 2)(13)
  
  pdf(paste0("../output/map_ltbis_",nari,"_",pp,".pdf"))
  mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbis", catMethod = seq(0,50,10),
                              colourPalette = cols,
                              addLegend = FALSE)
  do.call( addMapLegend, c( mapParams
                            , legendLabels="all"
                            , legendWidth=0.5
                            
  ))
  dev.off()
  
  pdf(paste0("../output/map_ltbir_",nari,"_",pp,".pdf"))
  mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbir", catMethod = seq(0,3,0.25),
                              colourPalette = cols,
                              addLegend = FALSE)
  do.call( addMapLegend, c( mapParams
                            , legendLabels="all"
                            , legendWidth=0.5
                            
  ))
  dev.off()
  
} 

### BEST fit using LOO
linm <- read.csv(paste0("../output/s_level_",nari,"_lin.csv"))[,-1]
quadm <- read.csv(paste0("../output/s_level_",nari,"_quad.csv"))[,-1]
quadcm <- read.csv(paste0("../output/s_level_",nari,"_quadc.csv"))[,-1]
linm$mod <- 1
quadm$mod <- 2
quadcm$mod <- 3

s_level_all <- rbind( linm, quadm, quadcm)

elpd <- read.csv( "../output/compare_models_elpd.csv")

for(i in 1:llu){
  cnn <- cni[i]
  
  w <- which(elpd$iso3 == cnn)
  ee <- elpd[w,]
  
  if(length(which(w_data$iso3 == cnn)) < 3){ # Can't be quad
    w2<-which(ee$X == "quad_loo")
    ee <- ee[-w2,]
  }
  
  max.elpd.level <- ee[which.max(ee$elpd_loo),"X"]
  if(max.elpd.level == "lin_loo"){lev <- 1}
  if(max.elpd.level == "quad_loo"){lev <- 2}
  if(max.elpd.level == "quadc_loo"){lev <- 3}
  
  ws <- intersect(which(as.character(s_level_all$pop_name) ==cnn), which(s_level_all$mod == lev))
  s_best <- s_level_all[ws,]
  s_best$model <- lev
  
  s_level_best <- rbind(s_level_best, s_best)
  
}

write.csv(s_level_best, paste0("../output/s_level_",nari,"best.csv"))


#### Run plots for s_level_best
pp <- "best"
s_level <- read.csv(paste0("../output/s_level_",nari,"_best.csv"))[,-1]

###********** PLOTS *****************************************************************#####
a2r<-ggplot(s_level, aes(x=pop_name, y = ltbir, col=factor(rep) )) + geom_point() + 
  guides(colour=FALSE) + 
  scale_x_discrete("Country") + scale_y_continuous("LTBI-MDR\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbir),y=ltbir) 
a2r
save_plot(paste0("../output/ltbi_all_countries_r",nari,"_",pp,".pdf"), a2r, base_aspect_ratio = 1.5 )

a2s<-ggplot(s_level, aes(x=pop_name, y = ltbis, col=factor(rep) )) + geom_point() + 
  guides(colour=FALSE) + 
  scale_x_discrete("Country") + scale_y_continuous("LTBI-DS\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbis),y=ltbis) 
a2s
save_plot(paste0("../output/ltbi_all_countries_s",nari,"_",pp,".pdf"), a2s, base_aspect_ratio = 1.5 )

s_level_mean <- s_level %>%
  group_by(pop_name) %>% 
  summarise_at(c("ltbir","ltbis","pltbir","pltbis"),funs(mean)) 

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

s_level_mean <- merge(s_level_mean, s_level_lo, by = "pop_name")
s_level_mean <- merge(s_level_mean, s_level_hi, by = "pop_name")


a2r<-ggplot(s_level_mean, aes(x=pop_name, y = ltbir )) + geom_point() + 
  scale_x_discrete("") + scale_y_continuous("") + #LTBI-MDR\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbir),y=ltbir) +
  geom_errorbar(aes(min = ltbir.lo, max = ltbir.hi))
a2r
save_plot(paste0("../output/ltbi_all_countries_r_mean",nari,"_",pp,".pdf"), a2r, base_aspect_ratio = 0.8)

a2s<-ggplot(s_level_mean, aes(x=pop_name, y = ltbis )) + geom_point() +  
  scale_x_discrete("Country") + scale_y_continuous("LTBI-DS\n(% population infected)") +
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip() + aes(x=reorder(pop_name,ltbis),y=ltbis)  +
  geom_errorbar(aes(min = ltbis.lo, max = ltbis.hi))
a2s
save_plot(paste0("../output/ltbi_all_countries_s_mean",nari,"_",pp,".pdf"), a2s, base_aspect_ratio = 1.5 )

#### *** MAP **** ###
library(maps)
# define color buckets
cols = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043","grey")
s_level_mean$ltbis_c <- as.numeric(cut(s_level_mean$ltbis, c(0, 10, 20, 30, 40, 50,100)))
s_level_mean$ltbis_r <- as.numeric(cut(s_level_mean$ltbis, c(0, 1, 2, 3, 4, 5, 6)))

library(ggplot2)
library(rworldmap)
library(RColorBrewer)


mapped_data <- joinCountryData2Map(s_level_mean, joinCode = "ISO3", nameJoinColumn = "pop_name")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
cols <- colorRampPalette(brewer.pal(11,"Reds"), bias = 2)(13)

pdf(paste0("../output/map_ltbis_",nari,"_",pp,".pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbis", catMethod = seq(0,50,10),
                            colourPalette = cols,
                            addLegend = FALSE)
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5
                          
))
dev.off()

pdf(paste0("../output/map_ltbir_",nari,"_",pp,".pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbir", catMethod = seq(0,3,0.25),
                            colourPalette = cols,
                            addLegend = FALSE)
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5
                          
))
dev.off()




