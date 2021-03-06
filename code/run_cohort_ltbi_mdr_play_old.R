#### Run cohort_ltbi_mdr ## PLAY

### Libraries and ggplot theme
library(ggplot2)
theme_set(theme_bw())
library(plyr)
library(dplyr)
library(cowplot)
library(data.table)

### Home
home <- "~/Documents/LTBI_MDR/"
setwd(home)
output <- "~/Documents/LTBI_MDR/output"
source("code/cohort_ltbi_mdr.R") # loads function for underyling cohort model
source("code/poss_mdr_curves.R") # loads function for gen MDR curves

### ARI DS - saved from postARIanalysis.R
load("data/rundata_ind.Rdata")
#ggplot(rundata_ind, aes(x=year,y=ari,col=replicate)) + geom_point() ## look at

## ARI input
ari <- as.data.frame(matrix(0,81,2))
colnames(ari) <- c("ds","mdr")
## Constant DS ARI
ari$ds <- 0.01

## India 2016.perc_new_mdr = ~2%
ari$mdr <- c(rep(0,1970-1934),seq(0,0.02,length=(2014-1969))) * ari$ds
#ari$mdr <- 0
colnames(ari) <- c("ds","mdr")

#plot(seq(1934,2014,1),ari$ds,ylim=c(0,0.02),type="l")
#lines(seq(1934,2014,1),ari$mdr,col="red")

### Countries to explore
cn_list <- c("India","China","Japan","Ukraine")

## Population examples
load('data/POP2014.Rdata')  
pop1 <- POP2014[which(POP2014$area == "India"),"value"]
pop2 <- POP2014[which(POP2014$area == "China"),"value"]
pop3 <- POP2014[which(POP2014$area == "Japan"),"value"]
pop4 <- POP2014[which(POP2014$area == "Ukraine"),"value"]

## group and plot populations
popp <- as.data.frame(rbind(cbind(pop1,1),cbind(pop2,2),cbind(pop3,3),cbind(pop4,4)))
colnames(popp) <- c("psize","cn")
popp$cn <- cn_list[popp$cn]
popp$age <- seq(0,80,5)
pp <-ggplot(popp, aes(y = psize, x = age, colour = factor(cn))) + geom_line() + scale_color_discrete("Country") + scale_y_continuous("Population size") + scale_x_continuous("Age")
setwd(output)
ggsave("popsize_2014.pdf",height = 4, width = 8)

### Run model for example generate above
cc <- cohort_ltbi(ari, pop1) # India

## output 
combs <- cc$combs
ltbi_dr <- sum(combs$perc_dr) # percentage with dr infection
ltbi_ds <- sum(combs$perc_ds) # percentage with ds infection
ltbi_dr
ltbi_ds
cc$c_2014

##****** Run for different ARI plots **********############################################################################################################################
nari = 10

ari_store <- as.data.frame(matrix(0,nari*81,2))
colnames(ari_store) <- c("ds","mdr")

## 1. no MDR, ds constant
ari_store[1:81,"ds"] <- 0.01
ari_store[1:81,"mdr"] <- 0

## 2. MDR linear increase to 2%, ds constant
ari_store[82:162,"ds"] <- 0.01
ari_store[82:162,"mdr"] <-c(rep(0,1970-1934),seq(0,0.02,length=(2014-1969))) * ari$ds

## 3-10. MDR sigmoid increase to 2%, ds decreasing after 1970
sigm <- function(x,delta){ 0.01/(1+exp(-(x-delta)))} # max = 2% of 0.01
#plot(seq(1934,2014,1),sigm(seq(0,80,1),41))
# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
jump_point <- c(41,46,51,56,61,66,71,76)
for(i in 3:10){
  dd <- jump_point[i-2]
  ari_store[((i-1)*81 + 1):(i*81),"ds"] <-c(rep(0.01,1970-1934),seq(0.01,0.005,length=(2014-1969))) 
  ari_store[((i-1)*81 + 1):(i*81),"mdr"] <-sigm(seq(0,80,1),dd)
}
ari_store$rep <- rep(1:nari,each = 81)
ari_store$time <- seq(1934,2014,1)
a1<- ggplot(ari_store,aes(x=time,y=ds, group=rep, colour = factor(rep))) + geom_line() + geom_line(aes(x=time,y=mdr,group=rep,colour=factor(rep),linetype = "dashed")) +
  scale_color_discrete("ARI pattern") + guides(linetype = FALSE)


s_level <- c()

for(i in 1:nari){
  
  ari <- ari_store[((i-1)*81 + 1):(i*81),c("ds","mdr")]
  for(j in 1:4){
    if(j==1){cc <- cohort_ltbi(ari, pop1)}
    if(j==2){cc <- cohort_ltbi(ari, pop2)}
    if(j==3){cc <- cohort_ltbi(ari, pop3)}
    if(j==4){cc <- cohort_ltbi(ari, pop4)}
    
    combs <- cc$combs
    ltbi_dr <- sum(combs$perc_dr)
    ltbi_ds <- sum(combs$perc_ds)
    
    s_level <- rbind(s_level,c(i,ltbi_dr,ltbi_ds,j))
  }
  
}

s_level <- as.data.frame(s_level)
colnames(s_level) <- c("rep","ltbir","ltbis","popf")

a2<-ggplot(s_level, aes(x=rep, y = ltbir, col=factor(rep) )) + geom_point(aes(shape=factor(popf))) + guides(colour=FALSE) + scale_shape_discrete("Population", labels = c("India","China","Japan","Ukraine"))

#ss <- merge(s_level,ari_store, by = 'rep')

plot_grid(a1, a2, labels = c("A", "B"), align = "h")



##****** MDR ARI variation with constant DS ARI ****########################################################################################################################
nari = 18
ari_store <- as.data.frame(matrix(0,nari*81,2))
ari_rep_labels <- c("No MDR","Linear increase",
                    "Sigmoid peak 1975","Sigmoid peak 1980","Sigmoid peak 1985","Sigmoid peak 1990","Sigmoid peak 1995","Sigmoid peak 2000","Sigmoid peak 2005","Sigmoid peak 2010", 
                    "Gaus peak 1975","Gaus peak 1980","Gaus peak 1985","Gaus peak 1990","Gaus peak 1995","Gaus peak 2000","Gaus peak 2005","Gaus peak 2010")

# Functions for shape of MDR ARI
sigm <- function(x,delta,max,B=1){ max/(1+exp(-B*(x-delta)))} 
plot(seq(1934,2014,1),sigm(seq(0,80,1),41,0.02),type="l")
points(seq(1934,2014,1),sigm(seq(0,80,1),41,0.02,0.4),type="l")
abline(v = 1970,lty="dashed")
gaum <- function(x,delta,max){ max*exp(-0.5*((x-delta)/5)^2)}
#plot(seq(1934,2014,1),gaum(seq(0,80,1),41, 1))

setwd("output")

### ARI MDR
## 1. no MDR
ari_store[1:81,"ds"] <- 0.01
ari_store[1:81,"mdr"] <- 0

## 2. MDR linear increase to 2%, ds constant
ari_store[82:162,"ds"] <- 0.01
ari_store[82:162,"mdr"] <-c(rep(0,1970-1934),seq(0,0.02,length=(2014-1969))) * 0.01

## 3-10. MDR sigmoid increase to maximum DS ARI 

# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
jump_point <- c(41,46,51,56,61,66,71,76)
# Sigma and plateau
for(i in 3:10){
  dd <- jump_point[i-2]
  ari_store[((i-1)*81 + 1):(i*81),"ds"] <-0.01
  ari_store[((i-1)*81 + 1):(i*81),"mdr"] <-sigm(seq(0,80,1),dd,0.02) * ari_store[((i-1)*81 + 1):(i*81),"ds"] # multiple ari trend of DR by that of DS up to max percentage in data 
}
# Jump
for(i in 11:18){
  dd <- jump_point[i-10]
  ari_store[((i-1)*81 + 1):(i*81),"ds"] <-0.01
  ari_store[((i-1)*81 + 1):(i*81),"mdr"] <-gaum(seq(0,80,1),dd,0.02) * ari_store[((i-1)*81 + 1):(i*81),"ds"] # multiple ari trend of DR by that of DS up to max percentage in data 
}

ari_store$rep <- rep(1:nari,each = 81)
ari_store$time <- seq(1934,2014,1)
ari_store$ds <- ari_store$ds - ari_store$mdr ## MDR is not an additional ARI but part of the original DS ARI
arim <- melt(ari_store[,c("time","ds","mdr","rep")], id.vars = c("time","rep"))
w <- intersect(which(arim$rep > 10),which(arim$variable == "mdr"))

levels(arim$variable) <- c(levels(arim$variable),"mdr_gaus")
arim[w,"variable"] = "mdr_gaus"

a1 <- ggplot(arim,aes(x=time,y=value, group=rep, colour = factor(rep))) + geom_line() + scale_y_continuous("ARI") + 
  facet_wrap(~variable, scales = "free") + scale_colour_discrete("ARI pattern",breaks = seq(1,18,1),labels = ari_rep_labels) + guides(colour=guide_legend(ncol=2))
save_plot("mdr_ari_with_constant_dsari.pdf", a1, base_aspect_ratio = 2.5 )


##****** Run for different countries  **********############################################################################################################################
cn <- c("India","China","Japan","Ukraine")
cni <- c("IND","CHN","JPN","UKR")
setwd("output")

# ARI trend data
load("../data/rundata_ari.Rdata")
rundata$ari <- exp(rundata$lari)
level2014 <- c(); #breakdown proportions infected by age
s_level <- c(); #sum proportions infected 


# MDR data
# India, China, Japan, Ukraine, latest = max
mdr_perc_new <- c(0.02, 0.057, 0.007, 0.22)

# Number of scenarios
nari = 130

setwd("~/Documents/LTBI_MDR/output")

# Store all? 
store_all <- as.data.frame(matrix(0,4*nari*81*100,9))
runn <- 1

for(cci in 1:length(cn)){
  sa <- c() # store for this country
  ### ARI DS
  rdata <- as.data.table(rundata[which(rundata$iso3 == cni[cci]),])
  
  # Need average over all replicates
  rr <- ddply(rdata, .(year), summarize,  Av=mean(ari)) #rdata %>% group_by( year ) %>%  summarise( Av = mean( x = ari , na.rm = TRUE ) )
  p1 <- ggplot(rr, aes(x=year,y=Av)) + geom_point() + ggtitle(cn_list[cci]) + scale_y_continuous("ARI DS-TB")
  
  ## ARI MDR curves (x 130)
  ari_s <- ari_mdr(cni[cci],rr)
  
  arim <- melt(ari_s, id.vars = c("time","rep"))
  
  # delete below? 
  #levels(arim$variable) <- c(levels(arim$variable),"mdr_gaus")
  #arim[w,"variable"] = "mdr_gaus"
  
  a1 <- ggplot(ari_s, aes(x=time, y = mdr, group = factor(rep))) + 
    geom_line(aes(col=factor(rep))) + facet_wrap(~type) + scale_y_continuous("ARI") + guides(color = FALSE)

  save_plot(paste0(cn_list[cci],".pdf"), a1, base_aspect_ratio = 2 )
  
  for(i in 1:nari){
    print(c(i,"ari rep"))
    ari <- ari_s[((i-1)*81 + 1):(i*81),c("ds","mdr")]
    
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
    
    s_level <- rbind(s_level,c(i,ltbi_dr,ltbi_ds,cci,pltbi_dr, pltbi_ds))
    
    ssc <- cc$store_c
    store_all[((runn-1)*(dim(ssc)[1])+1):(runn*(dim(ssc)[1])),] <- cbind(i,cci, ssc)
    runn <- runn + 1
    sa <- rbind(sa,cbind(i,cci, ssc)) # just for this country
  }
  
  sa <- as.data.frame(sa)
  colnames(sa) <- c(c("mdr_rep","cn"),colnames(cc$store_c))
  sa$age <- seq(1,100,1)
  sa_rec <- sa[which(sa$year > 1965),] # recent
  
  # #### Plot by age
  # g1<-ggplot(sa_rec, aes(x=year, y = pr_dr, group = age, col = age)) + geom_line() + 
  #   facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkorange1","darkgreen"))
  # ggsave(paste0("all_age_",cn_list[cci],"_r.pdf"), height = 10, width = 10)
  # 
  # g2<-ggplot(sa_rec, aes(x=year, y = pr_ds, group = age, col = age)) + geom_line() + 
  #   facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkmagenta", "darkgreen"))
  # ggsave(paste0("all_age_",cn_list[cci],"_s.pdf"), height = 10, width = 10)
  # 
  # g3<-ggplot(sa_rec, aes(year, age)) + geom_tile(aes(fill = pr_ds),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue","DS-TB") + 
  #   labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
  # ggsave(paste0("all_age_",cn_list[cci],"_map_s.pdf"), height = 10, width = 10)
  # 
  # g4<-ggplot(sa_rec, aes(year, age)) + geom_tile(aes(fill = pr_dr),colour = "white") + scale_fill_gradient(low = "white",high = "red4","MDR-TB") +
  #   labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
  # ggsave(paste0("all_age_",cn_list[cci],"_map_r.pdf"), height = 10, width = 10)
  # 
  s_level <- as.data.frame(s_level)
  colnames(s_level) <- c("rep","ltbir","ltbis","popf","pltbir", "pltbis")
  
  #ss <- merge(s_level,ari_store, by = 'rep')
  # bottom_row <- plot_grid(g3,g4, labels = c('B', 'C'), align = 'h', rel_widths = c(1, 1))
  # a<-plot_grid(a1, bottom_row, labels = c('A', ''), ncol = 1, rel_heights = c(1, 1.2))
  # save_plot(paste0("combined_",cn_list[cci],".pdf"), a, base_aspect_ratio = 2)
  # 
  # p.cn <- plot_grid(a1, a1, labels = c("A", "B"), align = "h",ncol = 2,rel_widths = c(1, 1.8))
  print(paste0(cn_list[cci]))
}
s_level0 <- s_level

s_level$popf <- cn_list[s_level$popf]
a2r<-ggplot(s_level, aes(x=rep, y = ltbir, col=factor(rep) )) + geom_point() + guides(colour=FALSE) + 
  scale_x_continuous("MDR-ARI trend") + scale_y_continuous("LTBI-MDR (% population infected)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~popf, scales = "free")
save_plot("ltbi_all_countries_r.pdf", a2r, base_aspect_ratio = 1.5 )

a2s<-ggplot(s_level, aes(x=rep, y = ltbis, col=factor(rep) )) + geom_point() + guides(colour=FALSE) + 
  scale_x_continuous("MDR-ARI trend") + scale_y_continuous("LTBI-DS (% population infected)",lim=c(0,(max(s_level$ltbis)+5))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~popf)
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
group_by(s_level,popf) %>% summarise(mm = mean(pltbis))


###******* AGE *****################################################################################################################
#ssc <- cc$store_c
#ssc$age <- seq(1,100,1)
#ggplot(ssc[3700:4400,],aes(x=year, y = pr_dr, group = age, col=age)) + geom_line() #+ facet_wrap(~age,scales = "free")

colnames(store_all) <- c(c("mdr_rep","cn"),colnames(cc$store_c))
store_all$age <- seq(1,100,1)
store_all_rec <- store_all[which(store_all$year > 1965),] # recent


for(iii in 1:length(cn)){
  store_all_c <- store_all_rec[which(store_all_rec$cn==iii),]
  
  ggplot(store_all_c, aes(x=year, y = pr_dr, group = age, col = age)) + geom_line() + 
    facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkorange1","darkgreen"))
  ggsave(paste0("all_age_",cn_list[iii],"_r.pdf"), height = 10, width = 10)
  
  ggplot(store_all_c, aes(x=year, y = pr_ds, group = age, col = age)) + geom_line() + 
    facet_wrap(~mdr_rep, ncol = 5) + scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkmagenta", "darkgreen"))
  ggsave(paste0("all_age_",cn_list[iii],"_s.pdf"), height = 10, width = 10)
  
  ggplot(store_all_c, aes(year, age)) + geom_tile(aes(fill = pr_ds),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue","DS-TB") + 
    labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
  ggsave(paste0("all_age_",cn_list[iii],"_map_s.pdf"), height = 10, width = 10)
  
  ggplot(store_all_c, aes(year, age)) + geom_tile(aes(fill = pr_dr),colour = "white") + scale_fill_gradient(low = "white",high = "red4","MDR-TB") +
    labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
  ggsave(paste0("all_age_",cn_list[iii],"_map_r.pdf"), height = 10, width = 10)
}


store_all_india <- store_all_rec[which(store_all_rec$cn==1),]
store_all_china <- store_all_rec[which(store_all_rec$cn==2),]
store_all_japan <- store_all_rec[which(store_all_rec$cn==3),]
store_all_ukraine <- store_all_rec[which(store_all_rec$cn==4),]


####*********** When contributes most to latent burden? *********######################################################################################################################################################
## Store_all has 4 countries by age and time
## 100 ages. 81 years. mdr_reps = 18. 
dim(store_all) # 4*18*100*81 = 583200
colnames(store_all)
## s_level has ltbir and ltbis levels by country (4) and mdr rep (18) (dim = 72)
dim(s_level)
colnames(s_level)
age_groups <- cbind(seq(1,85,5),seq(5,85,5))
age_groups[17,2] <- 100
uu <- unique(store_all$cn)
s_all<-c()
for(i in 1:max(uu)){ # for each country
  
  # subset: store_all has the proportion and the new / rei for each age / time
  s <-subset(store_all, cn == i) # all the new and rei for each age and time
  l14 <-subset(level2014, popf == i) # the proportions at 2014 in each age
  
  # where store?
  s_alloc_store<- c()
  
  s_new <- c()
  
  for(k in 1:18){# for each mdr_rep
    print(c("mdr_rep",k))
    # subset the yearly data 
    s_k <- subset(s, mdr_rep == k)
    
    for(j in 1:100){# all ages
     # print(c("age",j))
      
      # subset the proportion infected by mdr_rep and age group
      l14_k_j <- subset(l14, mdr_rep == k)[j,]
      
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
dim(s_all) # 4*18*100*81 = 583200


### Exploring plots
## cumr can go negative - re-infections v important. Proportion infected with R can decrease! 
# plot(s_temp[,"year"],s_temp[,"cumr_py"]) # total new each yar
# lines(s_temp[,"year"],s_temp[,"new_dr"]) # new infections 
# lines(s_temp[,"year"],s_temp[,"rei_sr"],col="blue") # additional reinfections
# lines(s_temp[,"year"],s_temp[,"rei_rs"],col="red") # removed reinfections with S
# abline(h = 0, lty = "dashed")

s_all_orig <- as.data.frame(s_all)
s_all <- as.data.frame(s_all)
colnames(s_all)<-c("pr_r","pr_s","year","age","mdr_rep","abs_pr_r","abs_pr_s","cn")
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

for(cci in 1:4){
  print(c("country",cci))
  
  ## For each country get population distribution
  if(cci==1){pop <- pop1}
  if(cci==2){pop <- pop2}
  if(cci==3){pop <- pop3}
  if(cci==4){pop <- pop4}
  
  ## For the population in 2014. Calculate the proportion of the total population in each yearly age group. 
  pr_2014_age = pop / sum(pop) / 5 ## Divided by 5 to make per subunit (think works as equivalent to averaging proportions and multiplying by total)
  
  ss_here <- subset(s_all, cn == cci) 
  
  for(i in 1:100){
    w <- which(ss_here$age == i)
    m <- intersect(which(age_groups[,1] <= i), which(age_groups[,2] >= i))
    ss_here[w,"pr_ltbir"] = ss_here[w,"pr_r"] * as.numeric(pr_2014_age[m])
    ss_here[w,"pr_ltbis"] = ss_here[w,"pr_s"] * as.numeric(pr_2014_age[m])
    ss_here[w,"abs_pr_ltbir"] = ss_here[w,"abs_pr_r"] * as.numeric(pr_2014_age[m])
    ss_here[w,"abs_pr_ltbis"] = ss_here[w,"abs_pr_s"] * as.numeric(pr_2014_age[m])
  }
  
  setwd(output)
  ## This says: by age, when were they infected. The proportion of their % infected that can be allocated to past times.
  ggplot(ss_here, aes(age, pr_s, fill = factor(year))) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous() + facet_wrap(~mdr_rep) + ggtitle(paste0("DS-TB, ",cn_list[cci])) +
    scale_fill_hue("clarity")
  ggsave(paste0("DS_age_",cn_list[cci],"_ltbis_when.pdf"), height = 10, width = 10)
  
  ggplot(ss_here, aes(age, pr_r, fill = factor(year))) +
    geom_bar(position = "fill", stat = "identity") + 
    scale_y_continuous() + facet_wrap(~mdr_rep) + ggtitle(paste0("MDR-TB, ",cn_list[cci])) + 
    scale_fill_hue("clarity")
  ggsave(paste0("DR_age_",cn_list[cci],"_ltbir_when.pdf"), height = 10, width = 10)
  
  
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
  ggsave(paste0("DS_",cn_list[cci],"_ltbis_when.pdf"), height = 10, width = 10)
  
  ggplot(ss_here) +
    geom_bar(aes(mdr_rep, pr_ltbir, fill = factor(yearcat)),
             position = "fill", stat = "identity") + 
    scale_y_continuous() + ggtitle("MDR-TB") +
    scale_fill_hue("Year")
  ggsave(paste0("DR_",cn_list[cci],"_ltbir_when.pdf"), height = 10, width = 10)
  
  ## Absolute contributions
  ggplot(ss_here) +
    geom_bar(aes(mdr_rep, abs_pr_ltbir, fill = factor(yearcat)),
             position = "fill", stat = "identity") + 
    scale_y_continuous() + ggtitle("MDR-TB") +
    scale_fill_hue("Year")
  ggsave(paste0("DR_",cn_list[cci],"_abs_ltbir_when.pdf"), height = 10, width = 10)
  
}

