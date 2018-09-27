### Combine results to give output

library(plyr)
library(xtable)
# Load in required data
load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes

for(ii in 1:4){# Three models and best
  
  # Labels for each model
  if(ii == 1){labl = "lin"}
  if(ii == 2){labl = "quad"}
  if(ii == 3){labl = "quadc"}
  if(ii == 4){labl = "best"}
  
  # number of replicates (up to 1000)
  nari <- 200
  
  # functions
  ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
  lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)
  
  # level at 2014
  s_level <- read.csv(paste0("~/Dropbox/MDR/cluster/s_level_",nari,"_",labl,".csv"))[,-1]
  s_level$iso3 <- s_level$pop_name
  
  ## How many at each level?
  if(ii == 4){
    n_level <- table(s_level$model) / 200
    # 1 = lin, 2 = quad, 3 = quadc (fixed 0 1970)
    unique(s_level[which(s_level$model == 1),"iso3"])
    unique(s_level[which(s_level$model == 2),"iso3"])
    unique(s_level[which(s_level$model == 3),"iso3"])
  }
  
  ### ADDITIONAL CALCULATIONS
  # Kid levels
  s_level$perc_ds_kids <- 100*s_level$pltbis_kids / s_level$pltbis
  s_level$perc_dr_kids <- 100*s_level$pltbir_kids / s_level$pltbir
  
  # Percentage of LTBI that is MDR
  s_level$perc_ltbi_mdr <- 100*s_level$pltbir/(s_level$pltbir + s_level$pltbis)
  
  #### By country
  med.ltbir <- aggregate(s_level[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids",
                                    "pltbir_kids","pltbis_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr")], 
                         list(s_level$iso3), median, na.rm = TRUE)
  
  ub.ltbir <- aggregate(s_level[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr")], 
                        list(s_level$iso3), ub)
  
  lb.ltbir <- aggregate(s_level[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr")], 
                        list(s_level$iso3), lb)
  
  med.ltbir$iso3 <- med.ltbir$Group.1
  ub.ltbir$iso3 <- ub.ltbir$Group.1
  lb.ltbir$iso3 <- lb.ltbir$Group.1
  
  #### By global region
  # Add in global regions
  ltbi <- merge(s_level,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
  
  # Sum over countries
  ltbi_global <- aggregate(ltbi[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")], 
                           list(ltbi$g_whoregion,ltbi$rep), sum)
  
  ltbi_global$perc_ds_kids <- 100*ltbi_global$pltbis_kids / ltbi_global$pltbis
  ltbi_global$perc_dr_kids <- 100*ltbi_global$pltbir_kids / ltbi_global$pltbir
  
  # Total global level
  ltbi_total <- aggregate(ltbi[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                  "pop","pop_kids")], 
                          list(ltbi$rep), sum) # still got replicates in there
  ltbi_total$perc_ds_kids <- 100*ltbi_total$pltbis_kids / ltbi_total$pltbis
  ltbi_total$perc_dr_kids <- 100*ltbi_total$pltbir_kids / ltbi_total$pltbir
  
  med.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                          "pop","pop_kids","perc_ds_kids","perc_dr_kids")],
                           list(ltbi_global$Group.1),median)
  
  ub.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                         "pop","pop_kids","perc_ds_kids","perc_dr_kids")],
                          list(ltbi_global$Group.1),ub)
  
  lb.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                         "pop","pop_kids","perc_ds_kids","perc_dr_kids")],
                          list(ltbi_global$Group.1),lb)
  
  med.total <- colwise(median)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids","perc_ds_kids","perc_dr_kids")])
  lb.total <- colwise(lb)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids","perc_ds_kids","perc_dr_kids")])
  ub.total <- colwise(ub)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids","perc_ds_kids","perc_dr_kids")])
  
  ## Global level
  med.ltbir.g$ltbir <- 100*med.ltbir.g$pltbir / med.ltbir.g$pop
  ub.ltbir.g$ltbir <- 100*ub.ltbir.g$pltbir / ub.ltbir.g$pop
  lb.ltbir.g$ltbir <- 100*lb.ltbir.g$pltbir / lb.ltbir.g$pop
  
  med.ltbir.g$ltbis <- 100*med.ltbir.g$pltbis / med.ltbir.g$pop
  ub.ltbir.g$ltbis <- 100*ub.ltbir.g$pltbis / ub.ltbir.g$pop
  lb.ltbir.g$ltbis <- 100*lb.ltbir.g$pltbis / lb.ltbir.g$pop
  
  med.ltbir.g$ltbir_kids <- 100*med.ltbir.g$pltbir_kids / med.ltbir.g$pop_kids
  ub.ltbir.g$ltbir_kids <- 100*ub.ltbir.g$pltbir_kids / ub.ltbir.g$pop_kids
  lb.ltbir.g$ltbir_kids <- 100*lb.ltbir.g$pltbir_kids / lb.ltbir.g$pop_kids
  
  med.ltbir.g$ltbis_kids <- 100*med.ltbir.g$pltbis_kids / med.ltbir.g$pop_kids
  ub.ltbir.g$ltbis_kids <- 100*ub.ltbir.g$pltbis_kids / ub.ltbir.g$pop_kids
  lb.ltbir.g$ltbis_kids <- 100*lb.ltbir.g$pltbis_kids / lb.ltbir.g$pop_kids
  
  med.ltbir.g$perc_ltbi_mdr <- 100*med.ltbir.g$pltbir / (med.ltbir.g$pltbir + med.ltbir.g$pltbis)
  ub.ltbir.g$perc_ltbi_mdr <- 100*ub.ltbir.g$pltbir / (ub.ltbir.g$pltbir + ub.ltbir.g$pltbis)
  lb.ltbir.g$perc_ltbi_mdr <- 100*lb.ltbir.g$pltbir / (lb.ltbir.g$pltbir + lb.ltbir.g$pltbis)
  
  ## Total levels
  med.total$ltbir <- 100*med.total$pltbir / med.total$pop
  ub.total$ltbir <- 100*ub.total$pltbir / ub.total$pop
  lb.total$ltbir <- 100*lb.total$pltbir / lb.total$pop
  
  med.total$ltbis <- 100*med.total$pltbis / med.total$pop
  ub.total$ltbis <- 100*ub.total$pltbis / ub.total$pop
  lb.total$ltbis <- 100*lb.total$pltbis / lb.total$pop
  
  med.total$ltbir_kids <- 100*med.total$pltbir_kids / med.total$pop_kids
  ub.total$ltbir_kids <- 100*ub.total$pltbir_kids / ub.total$pop_kids
  lb.total$ltbir_kids <- 100*lb.total$pltbir_kids / lb.total$pop_kids
  
  med.total$ltbis_kids <- 100*med.total$pltbis_kids /med.total$pop_kids
  ub.total$ltbis_kids <- 100*ub.total$pltbis_kids / ub.total$pop_kids
  lb.total$ltbis_kids <- 100*lb.total$pltbis_kids / lb.total$pop_kids
  
  med.total$perc_ltbi_mdr <- 100*med.total$pltbir / (med.total$pltbir + med.total$pltbis)
  ub.total$perc_ltbi_mdr <- 100*ub.total$pltbir / (ub.total$pltbir + ub.total$pltbis)
  lb.total$perc_ltbi_mdr <- 100*lb.total$pltbir / (lb.total$pltbir + lb.total$pltbis)
  
  #### Output tables
  ## All countries 
  table1_countries <- as.data.frame(cbind( as.character(med.ltbir$iso3),
                                           paste0(sprintf('%.1f',med.ltbir$ltbis), " [", 
                                                  sprintf('%.1f',lb.ltbir$ltbis), "-", 
                                                  sprintf('%.1f',ub.ltbir$ltbis),"]"),
                                           paste0(sprintf('%.1f',med.ltbir$perc_ds_kids), " [", 
                                                  sprintf('%.1f',lb.ltbir$perc_ds_kids), "-", 
                                                  sprintf('%.1f',ub.ltbir$perc_ds_kids),"]"),
                                           paste0(sprintf('%.2f',med.ltbir$ltbis_kids), " [", 
                                                  sprintf('%.2f',lb.ltbir$ltbis_kids), "-", 
                                                  sprintf('%.2f',ub.ltbir$ltbis_kids),"]"),
                                           paste0(sprintf('%.2f',med.ltbir$ltbir), " [", 
                                                  sprintf('%.2f',lb.ltbir$ltbir), "-", 
                                                  sprintf('%.2f',ub.ltbir$ltbir),"]"),
                                           paste0(sprintf('%.1f',med.ltbir$perc_dr_kids), " [", 
                                                  sprintf('%.1f',lb.ltbir$perc_dr_kids), "-", 
                                                  sprintf('%.1f',ub.ltbir$perc_dr_kids),"]"),
                                           paste0(sprintf('%.2f',med.ltbir$ltbir_kids), " [", 
                                                  sprintf('%.2f',lb.ltbir$ltbir_kids), "-", 
                                                  sprintf('%.2f',ub.ltbir$ltbir_kids),"]"),
                                           paste0(sprintf('%.1f',med.ltbir$perc_ltbi_mdr), " [", 
                                                  sprintf('%.1f',lb.ltbir$perc_ltbi_mdr), "-", 
                                                  sprintf('%.1f',ub.ltbir$perc_ltbi_mdr),"]")))
  
  colnames(table1_countries) <- c("iso3","Perc. with LTBIS", "Perc. of LTBIS in <15yos","Perc. of <15yos with LTBIS",
                                  "Perc. with LTBIR", "Perc. of LTBIR in <15yos","Perc. of <15yos with LTBIR","Perc. of LTBI that is MDR")
  
  
  ## Global
  table1_global <- as.data.frame(cbind( as.character(med.ltbir.g$Group.1),
                                        paste0(sprintf('%.1f',med.ltbir.g$ltbis), " [", 
                                               sprintf('%.1f',lb.ltbir.g$ltbis), "-", 
                                               sprintf('%.1f',ub.ltbir.g$ltbis),"]"),
                                        paste0(sprintf('%.1f',med.ltbir.g$perc_ds_kids), " [", 
                                               sprintf('%.1f',lb.ltbir.g$perc_ds_kids), "-", 
                                               sprintf('%.1f',ub.ltbir.g$perc_ds_kids),"]"),
                                        paste0(sprintf('%.2f',med.ltbir.g$ltbir), " [", 
                                               sprintf('%.2f',lb.ltbir.g$ltbir), "-", 
                                               sprintf('%.2f',ub.ltbir.g$ltbir),"]"),
                                        paste0(sprintf('%.1f',med.ltbir.g$perc_dr_kids), " [", 
                                               sprintf('%.1f',lb.ltbir.g$perc_dr_kids), "-", 
                                               sprintf('%.1f',ub.ltbir.g$perc_dr_kids),"]"),
                                        paste0(sprintf('%.1f',med.ltbir.g$perc_ltbi_mdr), " [", 
                                               sprintf('%.1f',lb.ltbir.g$perc_ltbi_mdr), "-", 
                                               sprintf('%.1f',ub.ltbir.g$perc_ltbi_mdr),"]")
  ))
  ## Total
  total <- as.data.frame(cbind("GLOBAL",
                               paste0(sprintf('%.1f',med.total$ltbis), " [", 
                                      sprintf('%.1f',lb.total$ltbis), "-", 
                                      sprintf('%.1f',ub.total$ltbis),"]"),
                               paste0(sprintf('%.1f',med.total$perc_ds_kids), " [", 
                                      sprintf('%.1f',lb.total$perc_ds_kids), "-", 
                                      sprintf('%.1f',ub.total$perc_ds_kids),"]"),
                               paste0(sprintf('%.2f',med.total$ltbir), " [", 
                                      sprintf('%.2f',lb.total$ltbir), "-", 
                                      sprintf('%.2f',ub.total$ltbir),"]"),
                               paste0(sprintf('%.1f',med.total$perc_dr_kids), " [", 
                                      sprintf('%.1f',lb.total$perc_dr_kids), "-", 
                                      sprintf('%.1f',ub.total$perc_dr_kids),"]"),
                               paste0(sprintf('%.1f',med.total$perc_ltbi_mdr), " [", 
                                      sprintf('%.1f',lb.total$perc_ltbi_mdr), "-", 
                                      sprintf('%.1f',ub.total$perc_ltbi_mdr),"]")
  ))
  
  table1_global <- rbind(table1_global, total)
  
  colnames(table1_global) <- c("WHO Region","Perc. with LTBIS", "Perc. of <15yos with LTBIS",
                               "Perc. with LTBIR", "Perc. of <15yos with LTBIR","Perc. of LTBI that is MDR")
  table1_global <- table1_global[c(1,2,5,3,6,4,7),]
  
  ## Store
  print(xtable(table1_countries), include.rownames=FALSE)
  print(xtable(table1_global), include.rownames=FALSE)
  
  write.csv(table1_countries, paste0("~/Dropbox/MDR/paper_output/table1_countries_",labl,".csv"))
  write.csv(table1_global, paste0("~/Dropbox/MDR/paper_output/table1_global_",labl,".csv"))
  
  if(ii == 1){med.ltbir.g.lin <- med.ltbir.g; med.total.lin <- med.total}
  if(ii == 2){med.ltbir.g.quad <- med.ltbir.g; med.total.quad <- med.total}
  if(ii == 3){med.ltbir.g.quadc <- med.ltbir.g; med.total.quadc <- med.total}
  if(ii == 4){med.ltbir.g.best <- med.ltbir.g; med.total.best <- med.total}
  
  
}

rs1<-round(100*(med.ltbir.g.best$ltbis - med.ltbir.g.lin$ltbis)/med.ltbir.g.best$ltbis,2)
rr1<-round(100*(med.ltbir.g.best$ltbir - med.ltbir.g.lin$ltbir)/med.ltbir.g.best$ltbir,2)

rs2<-round(100*(med.ltbir.g.best$ltbis - med.ltbir.g.quad$ltbis)/med.ltbir.g.best$ltbis,2)
rr2<-round(100*(med.ltbir.g.best$ltbir - med.ltbir.g.quad$ltbir)/med.ltbir.g.best$ltbir,2)

rs3<-round(100*(med.ltbir.g.best$ltbis - med.ltbir.g.quadc$ltbis)/med.ltbir.g.best$ltbis,2)
rr3<-round(100*(med.ltbir.g.best$ltbir - med.ltbir.g.quadc$ltbir)/med.ltbir.g.best$ltbir,2)


tt <- c(as.character("Global"),
        round(100*(med.total.best$ltbis - med.total.lin$ltbis)/med.total.best$ltbis,2),
        round(100*(med.total.best$ltbis - med.total.quad$ltbis)/med.total.best$ltbis,2),
        round(100*(med.total.best$ltbis - med.total.quadc$ltbis)/med.total.best$ltbis,2),
        round(100*(med.total.best$ltbir - med.total.lin$ltbir)/med.total.best$ltbir,2),
        round(100*(med.total.best$ltbir - med.total.quad$ltbir)/med.total.best$ltbir,2),
        round(100*(med.total.best$ltbir - med.total.quadc$ltbir)/med.total.best$ltbir,2))


tb <- rbind(cbind(as.character(med.ltbir.g[,1]), 
            rs1,rs2,rs3,rr1,rr2,rr3), tt)
colnames(tb) <- c("WHO region","best-lin ds","best-quad ds","best-quadc ds","best-lin dr","best-quad dr","best-quadc dr")
write.csv(tb, "~/Dropbox/MDR/paper_output/compare_linquadquadc.csv")
print(xtable(tb), include.rownames=FALSE)


#### *** MAP **** ###
### Map of kids ltbi prevalence
library(maps)
library(ggplot2)
library(rworldmap)
library(RColorBrewer)

# define color buckets
# if run above this will be the best one
med.ltbir$ltbir_kids_c <- as.numeric(cut(med.ltbir$ltbir_kids, c(0, 10, 20, 30, 40, 50, 100)))
med.ltbir$ltbis_kids_c <- as.numeric(cut(med.ltbir$ltbis_kids, c(0, 10, 20, 30, 40, 50, 60)))

mapped_data <- joinCountryData2Map(med.ltbir, joinCode = "ISO3", nameJoinColumn = "Group.1")

pdf(paste0("~/Dropbox/MDR/output/map_ltbis_kids_",nari,"_best.pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbis_kids", catMethod = seq(0,10,0.25),
                            colourPalette = cols,
                            addLegend = FALSE)
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5))
dev.off()

pdf(paste0("~/Dropbox/MDR/output/map_ltbir_kids_",nari,"_best.pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbir_kids", catMethod = seq(0,10,0.25),
                            colourPalette = cols,
                            addLegend = FALSE)
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5))
dev.off()

pdf(paste0("~/Dropbox/MDR/output/map_ltbir_",nari,"_best.pdf"))
mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "ltbir", catMethod = seq(0,10,0.25),
                            colourPalette = cols,
                            addLegend = FALSE)
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5))
dev.off()


#### *** LEVEL in 2035 / 2050 **** ###
load('data/POP2035.Rdata')
load('data/POP2050.Rdata')

N2035 <- sum(POP2035[,"value"])
POP2035$acatn <- 0:16
POP2035$acat <- POP2035$acatn-4
POP2035 <- subset(POP2035, POP2035$acat>=0)
POP2035$pop35 = POP2035$value
POP2035 <- POP2035[,c("iso3","acat","pop35")]

N2050 <- sum(POP2050[,"value"])
POP2050$acatn <- 0:16
POP2050$acat <- POP2050$acatn-7
POP2050 <- subset(POP2050, POP2050$acat>=0)
POP2050$pop50 = POP2050$value
POP2050 <- POP2050[,c("iso3","acat","pop50")]

fac <- 1e5*(0.15*1e-2)                  #reactivation rate

## 2035
# need iso, replicate, act, prev, g_whoregion 
fruns <- merge(rundatar[,list(iso3,replicate,acat,P,g_whoregion)],
               POP2035,by=c('iso3','acat'),all=TRUE)
fruns <- fruns[!is.na(pop35),]       #ditch the dead
tmp <- fruns[,list(nLTBI=sum(pop35*P)),by=replicate]
tmp <- tmp[!is.na(replicate)]
tmp[,list(mid=1e3*median(nLTBI),lo=1e3*lb(nLTBI),hi=1e3*ub(nLTBI))]


fac*tmp[,list(mid=median(nLTBI)/N2035,lo=lb(nLTBI)/N2035,hi=ub(nLTBI)/N2035)]

## 2050
fruns <- merge(rundatar[,list(iso3,replicate,acat,P,g_whoregion)],
               POP2050,by=c('iso3','acat'),all=TRUE)
fruns <- fruns[!is.na(pop50),]       #ditch the dead
tmp <- fruns[,list(nLTBI=sum(pop50*P)),by=replicate]
tmp <- tmp[!is.na(replicate)]
tmp[,list(mid=1e3*median(nLTBI),lo=1e3*lb(nLTBI),hi=1e3*ub(nLTBI))]

fac*tmp[,list(mid=median(nLTBI)/N2050,lo=lb(nLTBI)/N2050,hi=ub(nLTBI)/N2050)]

