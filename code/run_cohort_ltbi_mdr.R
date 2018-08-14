#### Run cohort_ltbi_mdr 


###********** Libraries and ggplot theme ************************************************************************************************#######
library(ggplot2)
theme_set(theme_bw(base_size = 24))
library(plyr)
library(dplyr)
library(cowplot)
library(data.table)
library(reshape2)

###********** Home ************************************************************************************************#######
home <- "~/Documents/LTBI_MDR/"
setwd(home)
#output <- "~/Documents/LTBI_MDR/output"
output <- "~/Dropbox/MDR/output" # TEMPORARY - can be shared between computers then

###********** Load code and data ************************************************************************************************#######
source("code/cohort_ltbi_mdr.R") # loads function for underyling cohort model
source("code/poss_mdr_curves.R") # loads function for gen MDR curves

## Population size 2014
load('data/POP2014.Rdata')  

###********** Example run for India ************************************************************************************************
### ARI DS - saved from postARIanalysis.R (H&D)
load("data/rundata_ind.Rdata") # For India
# ggplot(rundata_ind, aes(x=year,y=ari,col=replicate, group = replicate)) + geom_point() ## look at

### Run model for example 
pop1 <- POP2014[which(POP2014$area == "India"),"value"]
## ARI input
ari <- as.data.frame(matrix(0,81,2)); colnames(ari) <- c("ds","mdr")
## Constant DS ARI
ari$ds <- 0.01
## India 2016.perc_new_mdr = ~2%
ari$mdr <- c(rep(0,1970-1934),seq(0,0.02,length=(2014-1969))) * ari$ds

# Run model
cc <- cohort_ltbi(ari, pop1) # India

## output 
combs <- cc$combs
ltbi_dr <- sum(combs$perc_dr) # percentage with dr infection
ltbi_ds <- sum(combs$perc_ds) # percentage with ds infection
ltbi_dr
ltbi_ds
cc$c_2014

###********** Run for different countries ************************************************************************************************************************#######

setwd(output)

## Read in MDR curves
save_curves0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent_MDR/Data/lin_sig_quad_sigd_curves.csv",stringsAsFactors = FALSE)[-1]
mdr_cn_best <- read.csv("~/Documents/LTBI_MDR/output/store_cn_best.csv", stringsAsFactors = FALSE)[-1]
if(min(mdr_cn_best$Type) == 0){mdr_cn_best$Type = mdr_cn_best$Type + 1}  # make sure types 1 2 3 4 to match save_curves

## ARI trend data for DS TB
load("~/Documents/LTBI_MDR/data/rundata_ari.Rdata")
rundata$ari <- exp(rundata$lari)
level2014 <- c(); #breakdown proportions infected by age
s_level <- c(); #sum proportions infected 

## WHO data
w_data <- read.csv("~/Documents/LTBI_MDR/datar/new_who_edited.csv")

## Which countries? 
cni <- read.csv("~/Documents/LTBI_MDR/datar/107_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
length(cni) # 107
cni_rem <- c() # blank to store what else to remove

# Store all? 
store_all <- as.data.frame(matrix(0,length(cni)*4*81*100,9))
runn <- 1
nari = 4 # just 4 best

# Run for all countries
for(cci in 1:length(cni)){
  sa <- c() # store for this country
  print(c(cci,cni[cci]))
  
  ### WHO data
  d <-subset(w_data, iso3 == cni[cci] )
  
  # Remove if only one data point
  if(dim(d)[1] < 2){print(c("not enough data",cni[cci])); cni_rem = c(cni_rem, cci);
  } else {
    
    ### ARI DS
    rdata <- as.data.table(rundata[which(rundata$iso3 == cni[cci]),])
    
    # Need average over all replicates
    rr <- ddply(rdata, .(year), summarize,  Av=median(ari)) #rdata %>% group_by( year ) %>%  summarise( Av = mean( x = ari , na.rm = TRUE ) )
    #p1 <- ggplot(rr, aes(x=year,y=Av)) + geom_point() + ggtitle(cni[cci]) + scale_y_continuous("ARI DS-TB")
    
    ## ARI MDR curves (x 4)
    ari_s <- ari_mdr(cni[cci],rr,mdr_cn_best, save_curves0) # gives ari for ds and mdr (mdr = f_l curve * ds)
    
    arim <- melt(ari_s, id.vars = c("year","mdr"))
    
    a1 <- ggplot(ari_s, aes(x=year, y = mdr_c, group = factor(rep))) + 
      geom_line(aes(col=factor(best))) + facet_wrap(~type) + scale_y_continuous("Proportion new TB that is MDR") + 
      guides(color = FALSE) + geom_point( data = d , aes(x=year_new, y = mdr_new_pcnt/100, group = country)) + 
      scale_colour_manual(values = c("black","red"))
    save_plot(paste0(cni[cci],"_mdr_trends_4.pdf"), a1, base_aspect_ratio = 2 )
    
    a2 <- ggplot(ari_s, aes(x=year, y = mdr_c, group = factor(rep))) + 
      geom_line(aes(col=factor(rep), lty = factor(best)))  + scale_y_continuous("Proportion new TB that is MDR")+
      geom_point( data = d , aes(x=year_new, y = mdr_new_pcnt/100, group = country))+
      theme(legend.key.size = unit(2, 'lines')) + scale_colour_discrete("MDR Trend", labels = c(1,2,3,4)) +
      scale_linetype_manual("Best fit",values=c("dotted", "solid"), labels = c("Best fit\nfor trend only","Best fit\nacross trends"))
    save_plot(paste0(cni[cci],"_mdr_trends_best.pdf"), a2, base_aspect_ratio = 2 )
    
    a3 <- ggplot(ari_s, aes(x=year, y = mdr, group = factor(rep))) + 
      geom_line(aes(col=factor(rep), lty = factor(best)))  + scale_y_continuous("MDR ARI")+
      theme(legend.key.size = unit(2, 'lines')) + scale_colour_discrete("MDR Trend", labels = c(1,2,3,4)) +
      scale_linetype_manual("Best fit",values=c("dotted", "solid"), labels = c("Best fit\nfor trend only","Best fit\nacross trends"))
    save_plot(paste0(cni[cci],"_mdr_ari_trends_4.pdf"), a3, base_aspect_ratio = 2 )
    
    a4 <- ggplot(ari_s, aes(x=year, y = ds)) + geom_line()  + scale_y_continuous("Median DS ARI")
    save_plot(paste0(cni[cci],"_ds_trends.pdf"), a4, base_aspect_ratio = 2 )
    
    
    for(i in 1:nari){
      print(c(i,"ari rep"))
      ari <- ari_s[((i-1)*81 + 1):(i*81),c("ds","mdr")]
      best_v <- ari_s[((i-1)*81 + 1):(i*81),"best"][1]
      pop <-  POP2014[which(POP2014$iso3 == cni[cci]),"value"]
      
      cc <- cohort_ltbi(ari, pop1)
      
      combs <- cc$combs
      
      # by age 
      combs$mdr_rep <- i
      combs$age_group <- seq(1:17)
      combs$popf <- cci
      #level2014 <- rbind(level2014,combs)
      level2014 <- rbind(level2014,cbind(cc$c_2014,combs[1,c("mdr_rep","popf")],row.names = NULL))
      
      # total percentage infected sums
      ltbi_dr <- sum(combs$perc_dr) # percentage infected
      ltbi_ds <- sum(combs$perc_ds)
      pltbi_dr <- sum(combs$size_dr) # number of people infected
      pltbi_ds <- sum(combs$size_ds)
      
      # Bind together. best_v is 1 if this type was the best fit
      s_level <- rbind(s_level,c(i,ltbi_dr,ltbi_ds,cci,pltbi_dr, pltbi_ds, best_v))
      
      ssc <- cc$store_c
      lowi <- ((runn-1)*(dim(ssc)[1])+1)
      uppi <- ((runn)*(dim(ssc)[1]))
      store_all[lowi:uppi,1] <- i;
      store_all[lowi:uppi,2] <- cni[cci];
      store_all[lowi:uppi,3:9] <- ssc
      
      runn <- runn + 1
      sa <- rbind(sa,cbind(i,cni[cci], ssc)) # just for this country
    }
    
    sa <- as.data.frame(sa)
    colnames(sa) <- c(c("mdr_rep","cn"),colnames(cc$store_c))
    sa$age <- seq(1,100,1)
    sa_rec <- sa[which(sa$year > 1965),] # recent
    
    # #### Plot by age
    #g1<-ggplot(sa_rec, aes(x=year, y = pr_dr, group = age, col = age)) + geom_line() +
    #  facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkorange1","darkgreen"))
    #ggsave(paste0("all_age_",cni[cci],"_r.pdf"), height = 10, width = 10)
    
    #g2<-ggplot(sa_rec, aes(x=year, y = pr_ds, group = age, col = age)) + geom_line() +
    #  facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkmagenta", "darkgreen"))
    #ggsave(paste0("all_age_",cni[cci],"_s.pdf"), height = 10, width = 10)
    
    g3<-ggplot(sa_rec, aes(year, age)) + geom_tile(aes(fill = pr_ds),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue","DS-TB") +
      labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
    ggsave(paste0("all_age_",cni[cci],"_map_s.pdf"), height = 10, width = 10)
    
    g4<-ggplot(sa_rec, aes(year, age)) + geom_tile(aes(fill = pr_dr),colour = "white") + scale_fill_gradient(low = "white",high = "red4","MDR-TB") +
      labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
    ggsave(paste0("all_age_",cni[cci],"_map_r.pdf"), height = 10, width = 10)
    
    s_level <- as.data.frame(s_level)
    colnames(s_level) <- c("rep","ltbir","ltbis","popf","pltbir", "pltbis","best")
    
    ss <- merge(s_level,ari_s, by = 'rep')
    bottom_row <- plot_grid(g3,g4, labels = c('B', 'C'), align = 'h', rel_widths = c(1, 1))
    a<-plot_grid(a1, bottom_row, labels = c('A', ''), ncol = 1, rel_heights = c(1, 1.2))
    save_plot(paste0("combined_",cni[cci],".pdf"), a, base_aspect_ratio = 2)
    
    #p.cn <- plot_grid(a1, a1, labels = c("A", "B"), align = "h",ncol = 2,rel_widths = c(1, 1.8))
  }
}

dim(level2014) #4*100*107
level2014$cn <- cni[as.numeric(level2014$popf)]
write.csv(level2014, "level2014.csv")

s_level0 <- s_level
s_level$pop_name <- cni[as.numeric(s_level0$popf)]
s_level <- as.data.table(s_level)
write.csv(s_level, "s_level_7.csv")

### OR READ IN
level2014 <- read.csv("level2014.csv",stringsAsFactors = FALSE)[,-1]
s_level <- read.csv("s_level_7.csv", stringsAsFactors = FALSE)[,-1]

###********** PLOTS *****************************************************************#####
a2r<-ggplot(s_level, aes(x=popf, y = ltbir, col=factor(rep) )) + geom_point() + guides(colour=FALSE) + 
  scale_x_discrete("MDR-ARI trend") + scale_y_continuous("LTBI-MDR (% population infected)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~rep, scales = "free")
save_plot("ltbi_all_countries_r.pdf", a2r, base_aspect_ratio = 1.5 )

a2s<-ggplot(s_level, aes(x=popf, y = ltbis, col=factor(rep) )) + geom_point() + guides(colour=FALSE) + 
  scale_x_discrete("MDR-ARI trend") + scale_y_continuous("LTBI-DS (% population infected)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~rep, scales = "free")
save_plot("ltbi_all_countries_s.pdf", a2s, base_aspect_ratio = 1.5 )

a2r<-ggplot(s_level, aes(x=rep, y = pltbir, col=factor(rep) )) + geom_point() + guides(colour=FALSE) + 
  scale_x_continuous("MDR-ARI trend") + scale_y_continuous("LTBI-MDR total population infected") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~popf, scales = "free")
save_plot("ltbi_pop_all_countries_r.pdf", a2r, base_aspect_ratio = 1.5 )

a2s<-ggplot(s_level, aes(x=rep, y = pltbis, col=factor(rep) )) + geom_point() + guides(colour=FALSE) + 
  scale_x_continuous("MDR-ARI trend") + scale_y_continuous("LTBI-DS total population infected",lim=c(0,(max(s_level$pltbis)+5))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~popf)
save_plot("ltbi_pop_all_countries_s.pdf", a2s, base_aspect_ratio = 1.5 )


### To match to supplementary output by H&D
# Mean levels for each country
# Include all 
s_mean <- s_level #%>% group_by(pop_name,rep) %>% summarise_at(c("ltbir","ltbis","pltbir","pltbis"),funs(mean))
# Linear trend only
ggplot(subset(s_mean,rep == "1"), aes(x=pop_name, y = ltbir, col=ltbir)) + geom_point() + scale_y_continuous("LTBI-MDR percentage infected") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("ltbir_mean_all_countries_points_linear_trend.pdf")

## Plot all rep
ms_mean <- melt(s_mean[,c("pop_name","rep","ltbir","ltbis")], id.vars = c("pop_name","rep"))
ggplot(subset(ms_mean, variable == "ltbis"), aes(x=pop_name, y = value, fill = factor(rep))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(pop_name,X = value*(variable == "ltbis"))) + 
  scale_fill_discrete("MDR\nARI\ntrend") + 
  scale_x_discrete("Country") + scale_y_continuous("Percentage of population") +
  theme(axis.text.y = element_text(colour="grey20",size=6))
ggsave("ltbi_by_ls_all_rep.pdf")

ggplot(subset(ms_mean, variable == "ltbir"), aes(x=pop_name, y = value, fill = factor(rep))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(pop_name,X = value*(variable == "ltbir"))) + 
  scale_fill_discrete("MDR\nARI\ntrend") + 
  scale_x_discrete("Country") + scale_y_continuous("Percentage of population") +
  theme(axis.text.y = element_text(colour="grey20",size=6))
ggsave("ltbi_by_lr_all_rep.pdf")

## All quadratic
gbs <- ggplot(subset(ms_mean, rep ==1), aes(x=pop_name, y = value, fill = factor(variable))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(pop_name,X = value*(variable == "ltbis"))) + 
  scale_fill_discrete("LTBI level", labels = c("MDR","DS")) + 
  scale_x_discrete("Country") + scale_y_continuous("Percentage of population") +
  theme(axis.text.y = element_text(colour="grey20",size=6))
ggsave("ltbi_by_ls_lin.pdf")

gbs + geom_text(aes(label=round(value,2)), size = 2,position=position_dodge(width=0.9), hjust=-0.2) 
ggsave("ltbi_by_ls_lin_numbers.pdf")


gbr <- ggplot(subset(ms_mean, rep ==1), aes(x=pop_name, y = value, fill = factor(variable))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(pop_name,X = value*(variable == "ltbir"))) + 
  scale_fill_discrete("LTBI level", labels = c("MDR","DS")) +
  scale_x_discrete("Country") + scale_y_continuous("Percentage of population with LTBI-MDR") +
  theme(axis.text.y = element_text(colour="grey20",size=6))
ggsave("ltbi_by_lr_lin.pdf")

gbr + geom_text(aes(label=round(value,2)), size = 2,position=position_dodge(width=0.9), hjust=-0.2) + 
ggsave("ltbi_by_lr_lin_numbers.pdf")

ggplot(s_level, aes(x=pop_name, y = ltbir, col=factor(rep))) + geom_point() + 
  geom_line(aes(group = pop_name), col="black") + 
  scale_y_continuous("Percentage of population with LTBI-MDR") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = ltbir)) +
  scale_x_discrete("Country") + scale_color_discrete("MDR\nARI\ntrend") + 
  theme(axis.text.y = element_text(colour="grey20",size=6))
ggsave("ltbir_all_countries_all_trends_dots.pdf")

ggplot(subset(s_level, best == '1'), aes(x=pop_name, y = ltbir, col=factor(rep))) + geom_point() + 
  geom_line(aes(group = pop_name), col="black") + 
  scale_y_continuous("Percentage of population with LTBI-MDR") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = ltbir)) +
  scale_x_discrete("Country") + scale_color_discrete("Best\nMDR\nARI\ntrend") + 
  theme(axis.text.y = element_text(colour="grey20",size=6)) + geom_hline(yintercept = 1, lty = "dashed")
ggsave("ltbir_all_countries_best_trend_dot.pdf")

ggplot(s_level, aes(x=pop_name, y = ltbis, col=factor(rep))) + geom_point() + 
  geom_line(aes(group = pop_name), col="black") + 
  scale_y_continuous("Percentage of population with LTBI-DS") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = ltbis)) +
  scale_x_discrete("Country") + scale_color_discrete("MDR\nARI\ntrend") + 
  theme(axis.text.y = element_text(colour="grey20",size=6))
ggsave("ltbis_all_countries_all_trends_dots.pdf")

ggplot(subset(s_level, best == '1'), aes(x=pop_name, y = ltbis, col=factor(rep))) + geom_point() + 
  geom_line(aes(group = pop_name), col="black") + 
  scale_y_continuous("Percentage of population with LTBI-DS") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = ltbis)) +
  scale_x_discrete("Country") + scale_color_discrete("Best\nMDR\nARI\ntrend") + 
  theme(axis.text.y = element_text(colour="grey20",size=6))
ggsave("ltbis_all_countries_best_trend_dot.pdf")




s_types <- subset(s_level, best == '1') %>% count(rep) 
ggplot(s_types, aes(x=rep, y=n)) + geom_histogram(stat = "identity") + scale_x_continuous(breaks = c(1,2,3,4),labels = c("linear","sigmoid","quadratic","double sigmoid"),"Type of MDR trend")  
ggsave("types_mdr_trend_hist.pdf", width = 12, height = 9)

### Percentage difference by MDR ARI type
perc_diff <- c()
actual_diff <- c()
for(i in 1:length(cni)){
  dd <- subset(s_level, pop_name == cni[i])$ltbir
  perc_diff <- rbind(perc_diff, c(i,  100*(dd[1] - dd[2])/dd[1],  100*(dd[1] - dd[3])/dd[1],  100*(dd[1] - dd[4])/dd[1],
                                  100*(dd[2] - dd[3])/dd[2],  100*(dd[2] - dd[4])/dd[2],  100*(dd[3] - dd[4])/dd[3]))
  
  actual_diff <- rbind(actual_diff, c(i,  dd[1] - dd[2],  dd[1] - dd[3],  dd[1] - dd[4],dd[2] - dd[3],dd[2] - dd[4],  dd[3] - dd[4]))
                                  
}
colnames(perc_diff) <- c("cni","12","13","14","23","24","34")
colnames(actual_diff) <- c("cni","12","13","14","23","24","34")
perc_diff <- as.data.frame(abs(perc_diff))
actual_diff <- as.data.frame(abs(actual_diff))
perc_diff$max <- apply(perc_diff[,2:7], 1, max) 
perc_diff$pop_name <- cni[perc_diff$cni]

actual_diff$max <- apply(actual_diff[,2:7], 1, max) 
actual_diff$pop_name <- cni[actual_diff$cni]


ggplot(perc_diff, aes(x=pop_name, y = max)) + geom_point() +
  scale_y_continuous("Max percentage difference in LTBI-MDR between reps") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = max)) +
  scale_x_discrete("Country") +
  theme(axis.text.y = element_text(colour="grey20",size=6))

ggplot(actual_diff, aes(x=pop_name, y = max)) + geom_point() +
  scale_y_continuous("Max actual difference in LTBI-MDR between reps") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = max)) +
  scale_x_discrete("Country") +
  theme(axis.text.y = element_text(colour="grey20",size=6))


mperc_diff <- melt(perc_diff, id.vars = c("pop_name","max", "cni"))
ggplot(mperc_diff, aes(x=pop_name, y = value, col = variable)) + geom_point() +
  scale_y_continuous("Max percentage difference in LTBI-MDR between reps") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = max)) +
  scale_x_discrete("Country") +
  theme(axis.text.y = element_text(colour="grey20",size=6))

mperc_diff <- melt(perc_diff[,c("pop_name","max","cni","12","13","23")], id.vars = c("pop_name","max", "cni"))
ggplot(mperc_diff, aes(x=pop_name, y = value, col = variable)) + geom_point() +
  scale_y_continuous("Max percentage difference in LTBI-MDR between reps") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + aes(x=reorder(pop_name,X = max)) +
  scale_x_discrete("Country") +
  theme(axis.text.y = element_text(colour="grey20",size=6))


###******* AGE *****################################################################################################################
#ssc <- cc$store_c
#ssc$age <- seq(1,100,1)
#ggplot(ssc[3700:4400,],aes(x=year, y = pr_dr, group = age, col=age)) + geom_line() #+ facet_wrap(~age,scales = "free")

w<-which(store_all$V2 == 0)
store_all <- store_all[-w,]
colnames(store_all) <- c(c("mdr_rep","cn"),colnames(cc$store_c))
store_all$age <- seq(1,100,1)
write.csv(store_all,"store_all.csv")
store_all <- read.csv("store_all.csv")
store_all_rec <- store_all[which(store_all$year > 1965),] # recent


#Check
length(intersect(unique(level2014$cn), unique(store_all$cn))) # 107

#
# dim(store_all) # 4 countries *130 mdr_reps *100 ages * 81 years
# 
# for(iii in 1:length(cn)){
#   store_all_c <- store_all_rec[which(store_all_rec$cn==cn[iii]),]
#   
#   ggplot(store_all_c, aes(x=year, y = pr_dr, group = age, col = age)) + geom_line() + 
#     facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkorange1","darkgreen"))
#   ggsave(paste0("all_age_",cni[iii],"_r.pdf"), height = 10, width = 10)
#   
#   ggplot(store_all_c, aes(x=year, y = pr_ds, group = age, col = age)) + geom_line() + 
#     facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkmagenta", "darkgreen"))
#   ggsave(paste0("all_age_",cni[iii],"_s.pdf"), height = 10, width = 10)
#   
#   ggplot(store_all_c, aes(year, age)) + geom_tile(aes(fill = pr_ds),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue","DS-TB") + 
#     labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
#   ggsave(paste0("all_age_",cni[iii],"_map_s.pdf"), height = 10, width = 10)
#   
#   ggplot(store_all_c, aes(year, age)) + geom_tile(aes(fill = pr_dr),colour = "white") + scale_fill_gradient(low = "white",high = "red4","MDR-TB") +
#     labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
#   ggsave(paste0("all_age_",cni[iii],"_map_r.pdf"), height = 10, width = 10)
# }


# store_all_india <- store_all_rec[which(store_all_rec$cn==1),]
# store_all_china <- store_all_rec[which(store_all_rec$cn==2),]
# store_all_japan <- store_all_rec[which(store_all_rec$cn==3),]
# store_all_ukraine <- store_all_rec[which(store_all_rec$cn==4),]


####*********** When contributes most to latent burden? *********######################################################################################################################################################
## Store_all has 4 countries by age and time
## 100 ages. 81 years. mdr_reps = 4.Countries = 107
dim(store_all) # 107*4*100*81 = 3466800

colnames(store_all) <- c("rep","cn", colnames(ssc),"age")
## s_level has ltbir and ltbis levels by country (4) and mdr rep (18) (dim = 72)
dim(s_level)
colnames(s_level)
age_groups <- cbind(seq(1,85,5),seq(5,85,5))
age_groups[17,2] <- 100

s_all<-c()
for(i in 1:length(cni)){ # for each country
  
  # subset: store_all has the proportion and the new / rei for each age / time
  s <-subset(store_all, cn == cni[i]) # all the new and rei for each age and time
  l14 <-subset(level2014, cn == cni[i]) # the proportions at 2014 in each age
  
  # where store?
  s_alloc_store<- c()
  
  s_new <- c()
  
  for(k in 1:4){# for each mdr_rep
    print(c("mdr_rep",k))
    # subset the yearly data 
    s_k <- subset(s, rep == k)
    
    for(j in 1:100){# all ages
      # print(c("age",j))
      
      # subset the proportion infected by mdr_rep and age group
      l14_k_j <- subset(l14, mdr_rep == k)[j,] # proportion with dr in 2014 for mdr_rep = k and age = j 
      
      s_temp <- c()
      # for each year get the data for those that are that age in 2014
      for(yr in 2014:1934){
        
        agen = j - (2014-yr) # age in that year
        
        if(agen > 0){ # need age > 0!
          s_temp <- rbind(s_temp,subset(s_k, year == yr)[agen,]) # remeber this is age in 2014  
        }
      }
      
      ## Cumulative change in proportion with DR or DS 
      ## OK for this to go negative... as suggests proportion is decreasing.
      s_temp$cumr_py <- s_temp$new_dr - s_temp$rei_rs + s_temp$rei_sr
      s_temp$cums_py <- s_temp$new_ds - s_temp$rei_sr + s_temp$rei_rs
      
      ### Gives the right proportion as in l14_k_j
      # colwise(sum)(s_temp)[,"new_dr"] - colwise(sum)(s_temp)[,"rei_rs"] + colwise(sum)(s_temp)[,"rei_sr"]
      # tail(s_temp,1)[,"pr_ds"] + colwise(sum)(s_temp)[,"new_ds"] - colwise(sum)(s_temp)[,"rei_sr"] + colwise(sum)(s_temp)[,"rei_rs"]
      # sum(s_temp$cumr_py)
      # sum(s_temp$cums_py)
      # l14_k_j
      
      ## proportion of amount in 2014 that is from this cumulative change 
      if(l14_k_j$pr_dr > 0){s_propr <- s_temp$cumr_py /l14_k_j$pr_dr}else{s_propr <- matrix(0,81,1)}
      s_props <- s_temp$cums_py / (l14_k_j$pr_ds - tail(s_temp,1)[,"pr_ds"])
      
      s_npropr <- matrix(0,81,1);
      s_nprops <- matrix(0,81,1);
      for(kk in 1:length(s_props)){
        s_npropr[kk] <-  s_propr[kk]
        s_nprops[kk] <-  s_props[kk]
      }
      
      #s_new <- rbind(s_new,(cbind(s_npropr, s_nprops, seq(2014,1934,-1),j,k)))
      ## should be 1
      #sum(s_props)
      #sum(s_propr)
      
      ## Absolute cumulative change - could use if think often have decline pr_dr... 
      ## here artefact of using odd Gaussian curves? 
      s_temp$abs_cumr_py <- abs(s_temp$cumr_py)
      s_temp$abs_cums_py <- abs(s_temp$cums_py)
      ## new total = sum of all changes in LTBI
      abs_total_r <- sum(s_temp$abs_cumr_py)
      abs_total_s <- sum(s_temp$abs_cums_py)
      
      
      if(abs_total_r > 0){abs_s_propr <- s_temp$abs_cumr_py /abs_total_r}else{abs_s_propr <- matrix(0,81,1)}
      abs_s_props <- s_temp$abs_cums_py / (abs_total_s - tail(s_temp,1)[,"pr_ds"])
      
      abs_s_npropr <- matrix(0,81,1);
      abs_s_nprops <- matrix(0,81,1);
      for(kk in 1:length(s_props)){
        abs_s_npropr[kk] <-  abs_s_propr[kk]
        abs_s_nprops[kk] <-  abs_s_props[kk]
      }
      
      s_new <- rbind(s_new,(cbind(s_npropr, s_nprops, seq(2014,1934,-1),j,k,abs_s_npropr, abs_s_nprops)))
      ## should be 1
      
      
    }
    
    
  }
  s_all <- rbind(s_all, cbind(s_new,i))
}

## All data
## countries * mdr_rep * age in 2014 * year
dim(s_all) # 107*4*100*81 = 3466800
s_all_orig <- as.data.frame(s_all)
s_all <- as.data.frame(s_all_orig)

colnames(s_all)<-c("pr_r","pr_s","year","age","mdr_rep","abs_pr_r","abs_pr_s","cn")

s_all$cn <- cni[s_all$cn]

## each row has the age in 2014 the year from which some contribution may come and the size of the contribution
write.csv(s_all,"s_all.csv")
s_all <- read.csv("s_all.csv", stringsAsFactors = FALSE)[,-1]

### Exploring plots
## cumr can go negative - re-infections v important. Proportion infected with R can decrease! 
# plot(s_temp[,"year"],s_temp[,"cumr_py"]) # total new each yar
# lines(s_temp[,"year"],s_temp[,"new_dr"]) # new infections
# lines(s_temp[,"year"],s_temp[,"rei_sr"],col="blue") # additional reinfections
# lines(s_temp[,"year"],s_temp[,"rei_rs"],col="red") # removed reinfections with S
# abline(h = 0, lty = "dashed")

### s_all => has the proportion from each year at each age for each mdr_rep
w<-intersect(intersect(which(s_all$cn == 1),which(s_all$age == 32)),which(s_all$mdr_rep == 1))
sum(s_all[w,"pr_r"]) # = 1 
sum(s_all[w,"pr_s"]) # = 1


## plot: cumulative bar chart
## x axis = age [0-100]
## y axis = proportion ltbir from each time point [0-1]
## facet by mdr rep  

## Want: of all LTBI, when was it gained? So need not just by age... but by total population. 
s_all$pr_ltbir <- 0
s_all$pr_ltbis <- 0
s_all$yearcat<-cut(s_all$year, seq(1929,2018,5))
s_all$abs_pr_ltbir <- 0
s_all$abs_pr_ltbis <- 0


#ss_all <- matrix(0,dim(s_all)[1],dim(s_all)[2])
ss_all<-c()

for(cci in 1:length(cni)){
  print(c("country",cni[cci]))
  
  ## For each country get population distribution
  pop <-  POP2014[which(POP2014$iso3 == cni[cci]),"value"]
  
  ## For the population in 2014. Calculate the proportion of the total population in each yearly age group. 
  pr_2014_age = pop / sum(pop) / 5 ## Divided by 5 to make per subunit (think works as equivalent to averaging proportions and multiplying by total)
  pr_2014_age[17] = pop[17] / sum(pop) / 20 ## Apart from last which is 20 yrs long
  
  ss_here <- subset(s_all, cn == cci) 
  
  for(i in 1:100){
    w <- which(ss_here$age == i)
    m <- intersect(which(age_groups[,1] <= i), which(age_groups[,2] >= i))
    ss_here[w,"pr_ltbir"] = ss_here[w,"pr_r"] * as.numeric(pr_2014_age[m])
    ss_here[w,"pr_ltbis"] = ss_here[w,"pr_s"] * as.numeric(pr_2014_age[m])
    ss_here[w,"abs_pr_ltbir"] = ss_here[w,"abs_pr_r"] * as.numeric(pr_2014_age[m])
    ss_here[w,"abs_pr_ltbis"] = ss_here[w,"abs_pr_s"] * as.numeric(pr_2014_age[m])
  }
  
  ## Grouped by mdr_rep
  #w<-which(ss_here$mdr_rep == 5)
  #sum(ss_here[w,"pr_ltbis"]) # = 1
  #sum(ss_here[w,"pr_ltbir"]) # = 1
  
  setwd(output)
  ## This says: by age, when were they infected. The proportion of their % infected that can be
  # allocated to past times.
  ggplot(ss_here, aes(age, pr_s, fill = factor(year))) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous() + facet_wrap(~mdr_rep) + ggtitle(paste0("DS-TB, ",cni[cci])) +
    scale_fill_hue("clarity")
  ggsave(paste0("DS_age_",cni[cci],"_ltbis_when.pdf"), height = 10, width = 10)

  ggplot(ss_here, aes(age, pr_r, fill = factor(year))) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous() + facet_wrap(~mdr_rep) + ggtitle(paste0("MDR-TB, ",cni[cci])) +
    scale_fill_hue("clarity")
  ggsave(paste0("DR_age_",cni[cci],"_ltbir_when.pdf"), height = 10, width = 10)


  ## This says: by mdr_rep, when is the time window that contributes most
  ## Tried to highlight 1980 period... but not working
  #w <- which(ss_here$yearcat == "(1989,1994]")
  #ss_here$extra_label_fill <- 0
  #ss_here[w,"extra_label_fill"] <- 1
  #scale_colour_manual( values = c( "1"="black","0" = "white"), guide = FALSE )

  ggplot(ss_here, aes(mdr_rep, pr_ltbis, fill = factor(yearcat))) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous() + ggtitle("DS-TB") +
    scale_fill_hue("Year")
  ggsave(paste0("DS_",cni[cci],"_ltbis_when.pdf"), height = 10, width = 10)

  ggplot(ss_here) +
    geom_bar(aes(mdr_rep, pr_ltbir, fill = factor(yearcat)),
             position = "fill", stat = "identity") +
    scale_y_continuous("Percentage of LTBI MDR from this 5 year time interval") + ggtitle("MDR-TB") + scale_x_continuous(breaks = c(1,2,3,4),labels = c("linear","sigmoid","quadratic","double sigmoid"),"Type of MDR trend") +
    scale_fill_hue("Year")
  ggsave(paste0("DR_",cni[cci],"_ltbir_when.pdf"), height = 10, width = 10)

  ## Absolute contributions
  ggplot(ss_here) +
    geom_bar(aes(mdr_rep, abs_pr_ltbir, fill = factor(yearcat)),
             position = "fill", stat = "identity") +
    scale_y_continuous() + ggtitle("MDR-TB") +
    scale_fill_hue("Year")
  ggsave(paste0("DR_",cni[cci],"_abs_ltbir_when.pdf"), height = 10, width = 10)

  ## Store
  ss_here$cn <- cni[cci]
  #dim(ss_here)
  ss_all <- rbind(ss_all, ss_here)
  #ss_all[((cci-1)*dim(ss_here)[1] + 1):(cci*dim(ss_here)[1]),] <- ss_here
  
}

ss_all <- as.data.table(ss_all)
sum_ss_all <- ss_all%>%group_by(mdr_rep,yearcat,cn) %>%summarise(sum_prltbi = sum(pr_ltbir)) # if get single number = because plyr loaded after dplyr (has it's own summarise which gives single number)
dim(sum_ss_all) # 17*4*107 = 7276
## Checks
#w<-intersect(which(sum_ss_all$cn == "ALB"), which(sum_ss_all$mdr_rep == 1))
#sum(sum_ss_all[w,"sum_prltbi"]) # 1

setwd(output)
write.csv(sum_ss_all, "props_ltbi_when.csv")

