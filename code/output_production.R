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
  
  s_level$perc_ds_kids_all <- 100*s_level$pltbis_kids / (s_level$pltbis_kids + s_level$pltbir_kids)
  s_level$perc_dr_kids_all <- 100*s_level$pltbir_kids / (s_level$pltbis_kids + s_level$pltbir_kids)
  
  # Percentage of LTBI that is MDR
  s_level$perc_ltbi_mdr <- 100*s_level$pltbir/(s_level$pltbir + s_level$pltbis)
  
  #### By country
  med.ltbir <- aggregate(s_level[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids",
                                    "pltbir_kids","pltbis_kids",
                                    "perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")], 
                         list(s_level$iso3), median, na.rm = TRUE)
  
  ub.ltbir <- aggregate(s_level[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids",
                                   "perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")], 
                        list(s_level$iso3), ub)
  
  lb.ltbir <- aggregate(s_level[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids",
                                   "perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")], 
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
  
  ltbi_global$perc_ds_kids_all <- 100*ltbi_global$pltbis_kids / (ltbi_global$pltbis_kids + ltbi_global$pltbir_kids)
  ltbi_global$perc_dr_kids_all <- 100*ltbi_global$pltbir_kids / (ltbi_global$pltbis_kids + ltbi_global$pltbir_kids)
  
  ltbi_global$perc_ltbi_mdr <- 100*ltbi_global$pltbir / (ltbi_global$pltbir + ltbi_global$pltbis)
  
  # Total global level
  ltbi_total <- aggregate(ltbi[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                  "pop","pop_kids")], 
                          list(ltbi$rep), sum) # still got replicates in there
  ltbi_total$perc_ds_kids <- 100*ltbi_total$pltbis_kids / ltbi_total$pltbis
  ltbi_total$perc_dr_kids <- 100*ltbi_total$pltbir_kids / ltbi_total$pltbir
  ltbi_total$perc_ds_kids_all <- 100*ltbi_total$pltbis_kids / (ltbi_total$pltbis_kids + ltbi_total$pltbir_kids)
  ltbi_total$perc_dr_kids_all <- 100*ltbi_total$pltbir_kids / (ltbi_total$pltbis_kids + ltbi_total$pltbir_kids)
  
  ltbi_total$perc_ltbi_mdr <- 100*ltbi_total$pltbir / (ltbi_total$pltbir + ltbi_total$pltbis)
  
  med.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                          "pop","pop_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")],
                           list(ltbi_global$Group.1),median)
  
  ub.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                         "pop","pop_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")],
                          list(ltbi_global$Group.1),ub)
  
  lb.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                         "pop","pop_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")],
                          list(ltbi_global$Group.1),lb)
  
  med.total <- colwise(median)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")])
  lb.total <- colwise(lb)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")])
  ub.total <- colwise(ub)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids","perc_ds_kids","perc_dr_kids","perc_ltbi_mdr","perc_ds_kids_all","perc_dr_kids_all")])
  
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
  
  #### Output tables
  med.ltbir <- merge(med.ltbir,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE) # add global region to country output
  ## All countries 
  table1_countries <- as.data.frame(cbind(as.character(med.ltbir$iso3),
                                           as.character(med.ltbir$g_whoregion),
                                           paste0(sprintf('%.1f',med.ltbir$ltbis), " [", 
                                                  sprintf('%.1f',lb.ltbir$ltbis), "-", 
                                                  sprintf('%.1f',ub.ltbir$ltbis),"]"),
                                           paste0(sprintf('%.2f',med.ltbir$ltbir), " [", 
                                                  sprintf('%.2f',lb.ltbir$ltbir), "-", 
                                                  sprintf('%.2f',ub.ltbir$ltbir),"]"),
                                           paste0(sprintf('%.1f',med.ltbir$perc_ltbi_mdr), " [", 
                                                  sprintf('%.1f',lb.ltbir$perc_ltbi_mdr), "-", 
                                                  sprintf('%.1f',ub.ltbir$perc_ltbi_mdr),"]"),
                                           paste0(sprintf('%.1f',med.ltbir$perc_dr_kids_all), " [", 
                                                  sprintf('%.1f',lb.ltbir$perc_dr_kids_all), "-", 
                                                  sprintf('%.1f',ub.ltbir$perc_dr_kids_all),"]")
                                           ))
  
  colnames(table1_countries) <- c("iso3","WHO region", "Perc. with LTBIS", "Perc. with LTBIR", 
                                  "Perc. of LTBI that is MDR","Perc. of LTBI in <15yos that is MDR")
  
  ## All countries: NUMBER
  table1_countries_numb <- as.data.frame(cbind( as.character(med.ltbir$iso3),
                                           as.character(med.ltbir$g_whoregion),
                                           paste0(sprintf('%.1f',1000 * med.ltbir$pltbis), " [", 
                                                  sprintf('%.1f',1000 * lb.ltbir$pltbis), "-", 
                                                  sprintf('%.1f',1000 * ub.ltbir$pltbis),"]"),
                                           paste0(sprintf('%.1f',1000 * med.ltbir$pltbir), " [", 
                                                  sprintf('%.1f',1000 * lb.ltbir$pltbir), "-", 
                                                  sprintf('%.1f',1000 * ub.ltbir$pltbir),"]"),
                                           paste0(sprintf('%.1f',1000 * (med.ltbir$pltbir+med.ltbir$pltbis)), " [", 
                                                  sprintf('%.1f',1000 * (lb.ltbir$pltbir+lb.ltbir$pltbis)), "-", 
                                                  sprintf('%.1f',1000 * (ub.ltbir$pltbir+ub.ltbir$pltbis)),"]")
                                           ))
  
  colnames(table1_countries_numb) <- c("iso3","WHO region", "Number with LTBIS","Number with LTBIR", "Total with LTBI")
  

  ## Global
  table1_global <- as.data.frame(cbind( as.character(med.ltbir.g$Group.1),
                                        paste0(sprintf('%.1f',med.ltbir.g$ltbis), " [", 
                                               sprintf('%.1f',lb.ltbir.g$ltbis), "-", 
                                               sprintf('%.1f',ub.ltbir.g$ltbis),"]"),
                                        paste0(sprintf('%.2f',med.ltbir.g$ltbir), " [", 
                                               sprintf('%.2f',lb.ltbir.g$ltbir), "-", 
                                               sprintf('%.2f',ub.ltbir.g$ltbir),"]"),
                                        paste0(sprintf('%.1f',med.ltbir.g$perc_ltbi_mdr), " [", 
                                               sprintf('%.1f',lb.ltbir.g$perc_ltbi_mdr), "-", 
                                               sprintf('%.1f',ub.ltbir.g$perc_ltbi_mdr),"]"),
                                        paste0(sprintf('%.1f',med.ltbir.g$perc_dr_kids_all), " [", 
                                               sprintf('%.1f',lb.ltbir.g$perc_dr_kids_all), "-", 
                                               sprintf('%.1f',ub.ltbir.g$perc_dr_kids_all),"]")
                                        ))
  ## Total
  total <- as.data.frame(cbind("GLOBAL",
                               paste0(sprintf('%.1f',med.total$ltbis), " [", 
                                      sprintf('%.1f',lb.total$ltbis), "-", 
                                      sprintf('%.1f',ub.total$ltbis),"]"),
                               paste0(sprintf('%.2f',med.total$ltbir), " [", 
                                      sprintf('%.2f',lb.total$ltbir), "-", 
                                      sprintf('%.2f',ub.total$ltbir),"]"),
                               paste0(sprintf('%.1f',med.total$perc_ltbi_mdr), " [", 
                                      sprintf('%.1f',lb.total$perc_ltbi_mdr), "-", 
                                      sprintf('%.1f',ub.total$perc_ltbi_mdr),"]"),
                               paste0(sprintf('%.1f',med.total$perc_dr_kids_all), " [", 
                                      sprintf('%.1f',lb.total$perc_dr_kids_all), "-", 
                                      sprintf('%.1f',ub.total$perc_dr_kids_all),"]")
                               ))
  
  table1_global <- rbind(table1_global, total)
  
  colnames(table1_global) <- c("WHO Region","Perc. with LTBIS", "Perc. with LTBIR", "Perc. of LTBI that is MDR", "Perc. of LTBI in <15yos that is MDR")
  table1_global <- table1_global[c(1,2,5,3,6,4,7),]
  
  ## Store
  print(xtable(table1_countries), include.rownames=FALSE)
  print(xtable(table1_global), include.rownames=FALSE)
  
  write.csv(table1_countries, paste0("~/Dropbox/MDR/paper_output/table1_countries_",labl,".csv"))
  write.csv(table1_countries_numb, paste0("~/Dropbox/MDR/paper_output/table1_countries_number_",labl,".csv"))
  write.csv(table1_global, paste0("~/Dropbox/MDR/paper_output/table1_global_",labl,".csv"))
  
  
  if(ii == 1){med.ltbir.g.lin <- med.ltbir.g; med.total.lin <- med.total}
  if(ii == 2){med.ltbir.g.quad <- med.ltbir.g; med.total.quad <- med.total}
  if(ii == 3){med.ltbir.g.quadc <- med.ltbir.g; med.total.quadc <- med.total}
  if(ii == 4){med.ltbir.g.best <- med.ltbir.g; med.total.best <- med.total}
  
  
}

## Check perc_ltbi_mdr greater variance than what comparing
rp <- ub.total$perc_ltbi_mdr - lb.total$perc_ltbi_mdr
rs <- ub.total$pltbis - lb.total$pltbis
rr <- ub.total$pltbir - lb.total$pltbir
rp / med.total$perc_ltbi_mdr # 0.4 vs 0.3 for other two below. Correct! should have greater variance as taking into variance in both
rs / med.total$pltbis
rr / med.total$pltbir

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
cols <- colorRampPalette(brewer.pal(11,"Reds"), bias = 2)(13)

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

cni <- read.csv("~/Dropbox/MDR/107_final_list_included_countries.csv", stringsAsFactors = FALSE)[,-1]
elpd <- read.csv( "~/Dropbox/MDR/output/compare_models_elpd.csv")
elpd_best <- c()

# Which level best for each country?
for(i in 1:107){
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
  
  elpd_best <- rbind(elpd_best, c(cnn, lev))
}

data_all <- c()
for(ii in 1:4){
  # Labels for each model
  if(ii == 1){labl = "lin"}
  if(ii == 2){labl = "quad"}
  if(ii == 3){labl = "quadc"}
  if(ii == 4){labl = "best"}
  
  datam <- as.data.frame(matrix(0,107*20000, 10))
  
  ### Gather data
  if(ii < 4){
    for(i in 1:107){
      cnn <- cni[i]
      d <- read.csv(paste0("~/Dropbox/MDR/cluster/",cnn,"_level2014_200_",labl,".csv"))[,-1]
      d$age <- seq(1,100,1)
      datam[(1 + (i-1)*20000):(i*20000),] <- d
    }
    colnames(datam) <- colnames(d)
    data_all <- rbind(data_all, cbind(datam,ii)) # to find best
  }
  
  if(ii == 4){
    for(i in 1:107){
      cnn <- cni[i]
      w<-which(data_all$cn == cnn)
      we <- which(elpd_best[,1] == cnn)
      # which best?
      lev <- elpd_best[we,2]
      w2 <- which(data_all$ii == lev)
      
      cn_lev_best <- intersect(w,w2)
      
      datam[(1 + (i-1)*20000):(i*20000),] <- data_all[cn_lev_best,]
    }
  }
  
  
  # 2035 population
  N2035 <- sum(POP2035[,"value"])
  POP2035$acatn <- 0:16
  POP2035$acat <- POP2035$acatn-4
  POP2035 <- subset(POP2035, POP2035$acat>=0)
  POP2035$pop35 = POP2035$value
  POP2035 <- POP2035[,c("iso3","acat","pop35")]
  
  # 2050 population 
  N2050 <- sum(POP2050[,"value"])
  POP2050$acatn <- 0:16
  POP2050$acat <- POP2050$acatn-7
  POP2050 <- subset(POP2050, POP2050$acat>=0)
  POP2050$pop50 = POP2050$value
  POP2050 <- POP2050[,c("iso3","acat","pop50")]
  
  #fac <- 1e5*(0.15*1e-2)                  #reactivation rate
  
  # Group by age categories
  datam$acat <- c(rep(seq(0,15,1), each = 5), rep(16,20))
  new_data <- datam[,c("pr_dr","pr_ds","mdr_rep","acat","popf")] %>% group_by(mdr_rep,acat,popf) %>% summarise_at(c("pr_dr","pr_ds"),funs(mean)) 
  dim(new_data) # 107*200*17 = 363800
  new_data$iso3 <- cni[new_data$popf]
  
  new_data2 <- merge(new_data,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
  
  ## 2035
  # need iso, replicate, act, prev, g_whoregion 
  fruns <- merge(new_data2[,c("iso3","mdr_rep","acat","pr_dr","pr_ds","g_whoregion")],
                 POP2035,by=c('iso3','acat'),all=TRUE)
  fruns <- fruns[!is.na(fruns$mdr_rep),]     # ditch those countries not in the 107
  fruns <- fruns[!is.na(fruns$pop35),]       # ditch the dead - only acat to 12 included
  
  fruns$pLTBIR <- 1000*fruns$pop35 * fruns$pr_dr # each number in POP2035 is the actual number divided by 1,000
  fruns$pLTBIS <- 1000*fruns$pop35 * fruns$pr_ds
  
  dim(fruns) ## 200*107*13 = 278200
  
  # fac*tmp[,list(mid=median(nLTBI)/N2035,lo=lb(nLTBI)/N2035,hi=ub(nLTBI)/N2035)]
  
  fruns.total.35 <- aggregate(fruns[,c("pLTBIR","pLTBIS")], 
                          list(fruns$mdr_rep), sum)
  med.fruns.total.35 <- colwise(median)(fruns.total) / 1e6 # per million
  ub.fruns.total.35 <- colwise(ub)(fruns.total) / 1e6
  lb.fruns.total.35 <- colwise(lb)(fruns.total) / 1e6
  
  print(c(paste0(sprintf('%.1f',med.fruns.total.35$pLTBIR)," [",
                         sprintf('%.1f',lb.fruns.total.35$pLTBIR),", ",
                                 sprintf('%.1f',ub.fruns.total.35$pLTBIR),"]")))
  print(c(paste0(sprintf('%.1f',med.fruns.total.35$pLTBIS)," [",
                         sprintf('%.1f',lb.fruns.total.35$pLTBIS),", ",
                                 sprintf('%.1f',ub.fruns.total.35$pLTBIS),"]")))
  
  
  med.rate.35 <- (0.15*0.01) * med.fruns.total.35 * 1e6/1e5 # 0.15% reactivation x total in millions per 100,000
  ub.rate.35 <- (0.15*0.01) * ub.fruns.total.35 * 1e6/1e5 # 0.15% reactivation x total in millions per 100,000
  lb.rate.35 <- (0.15*0.01) * lb.fruns.total.35 * 1e6/1e5 # 0.15% reactivation x total in millions per 100,000
  
  print(c(paste0(sprintf('%.1f',med.rate.35$pLTBIR)," [",
                 sprintf('%.1f',lb.rate.35$pLTBIR),", ",
                 sprintf('%.1f',ub.rate.35$pLTBIR),"]")))
  print(c(paste0(sprintf('%.1f',med.rate.35$pLTBIS)," [",
                 sprintf('%.1f',lb.rate.35$pLTBIS),", ",
                 sprintf('%.1f',ub.rate.35$pLTBIS),"]")))
  
  
  ## 2050
  # need iso, replicate, act, prev, g_whoregion 
  fruns <- merge(new_data2[,c("iso3","mdr_rep","acat","pr_dr","pr_ds","g_whoregion")],
                 POP2050,by=c('iso3','acat'),all=TRUE)
  fruns <- fruns[!is.na(fruns$mdr_rep),]     # ditch those countries not in the 107
  fruns <- fruns[!is.na(fruns$pop50),]       # ditch the dead - only acat to 12 included
  
  fruns$pLTBIR <- 1000*fruns$pop50 * fruns$pr_dr # each number in POP2050 is the actual number divided by 1,000
  fruns$pLTBIS <- 1000*fruns$pop50 * fruns$pr_ds
  
  dim(fruns) ## 200*107*13 = 278200
  
  # fac*tmp[,list(mid=median(nLTBI)/N2050,lo=lb(nLTBI)/N2050,hi=ub(nLTBI)/N2050)]
  
  fruns.total.50 <- aggregate(fruns[,c("pLTBIR","pLTBIS")], 
                              list(fruns$mdr_rep), sum)
  med.fruns.total.50 <- colwise(median)(fruns.total) / 1e6 # per million
  ub.fruns.total.50 <- colwise(ub)(fruns.total) / 1e6
  lb.fruns.total.50 <- colwise(lb)(fruns.total) / 1e6
  
  print(c(paste0(sprintf('%.1f',med.fruns.total.50$pLTBIR)," [",
                 sprintf('%.1f',lb.fruns.total.50$pLTBIR),", ",
                 sprintf('%.1f',ub.fruns.total.50$pLTBIR),"]")))
  print(c(paste0(sprintf('%.1f',med.fruns.total.50$pLTBIS)," [",
                 sprintf('%.1f',lb.fruns.total.50$pLTBIS),", ",
                 sprintf('%.1f',ub.fruns.total.50$pLTBIS),"]")))
  
  
  med.rate.50 <- (0.15*0.01) * med.fruns.total.50 * 1e6/1e5 # 0.15% reactivation x total in millions per 100,000
  ub.rate.50 <- (0.15*0.01) * ub.fruns.total.50 * 1e6/1e5 # 0.15% reactivation x total in millions per 100,000
  lb.rate.50 <- (0.15*0.01) * lb.fruns.total.50 * 1e6/1e5 # 0.15% reactivation x total in millions per 100,000
  
  print(c(paste0(sprintf('%.1f',med.rate.50$pLTBIR)," [",
                 sprintf('%.1f',lb.rate.50$pLTBIR),", ",
                 sprintf('%.1f',ub.rate.50$pLTBIR),"]")))
  print(c(paste0(sprintf('%.1f',med.rate.50$pLTBIS)," [",
                 sprintf('%.1f',lb.rate.50$pLTBIS),", ",
                 sprintf('%.1f',ub.rate.50$pLTBIS),"]")))
  
  #### UP TO HERE
  
  
  ## 2050
  fruns <- merge(rundatar[,list(iso3,replicate,acat,P,g_whoregion)],
                 POP2050,by=c('iso3','acat'),all=TRUE)
  fruns <- fruns[!is.na(pop50),]       #ditch the dead
  tmp <- fruns[,list(nLTBI=sum(pop50*P)),by=replicate]
  tmp <- tmp[!is.na(replicate)]
  tmp[,list(mid=1e3*median(nLTBI),lo=1e3*lb(nLTBI),hi=1e3*ub(nLTBI))]
  
  fac*tmp[,list(mid=median(nLTBI)/N2050,lo=lb(nLTBI)/N2050,hi=ub(nLTBI)/N2050)]
  
  