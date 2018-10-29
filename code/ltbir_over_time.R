#### LTBIR over time
## Have all the country data in output/out_year

library(plyr)
library(xtable)
library(magrittr)
library(dplyr)
library(ggplot2)
library("ggforce")
library(reshape)
library(wpp2017) # UN pop data

# Load in required data
load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes
cni <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv", stringsAsFactors = FALSE)[,-1]

# Just Infor_priors now
labl = "infor_prior"

# number of replicates (up to 1000)
nari <- 200

# functions
ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)

# UN pop data
## Population size 2014
load('~/Documents/LTBI_MDR/data/POP2014.Rdata')  
data(popM)
data(popF)
popM$gender <- 0
popF$gender <- 1
pop <- rbind(popM, popF)
mpop <- melt(pop, id.vars= c("country_code","name","age","gender"))
pop.totals <- aggregate(mpop[,c("value")], list(mpop$country_code, mpop$name, mpop$age, mpop$variable), sum)
colnames(pop.totals) <- c("country_code","country","age","year","popsize")
pop.totals$year <- seq(1950,2015,5)[as.numeric(pop.totals$year)] 
pop.totals2 <- pop.totals[which(pop.totals$year > 1969),]

## WHOkey match names tp those in pop.totals
levels(WHOkey$country) <- c(levels(WHOkey$country), "Cote d'Ivoire", "Czechia", "United Kingdom","TFYR Macedonia","Dem. People's Republic of Korea")
w <- which(WHOkey$iso3 == "CIV")
WHOkey[w,"country"] <- "Cote d'Ivoire" 

w <- which(WHOkey$iso3 == "CZE")
WHOkey[w,"country"] <- "Czechia" 

w <- which(WHOkey$iso3 == "GBR")
WHOkey[w,"country"] <- "United Kingdom" 

w <- which(WHOkey$iso3 == "MKD")
WHOkey[w,"country"] <- "TFYR Macedonia" 

w <- which(WHOkey$iso3 == "PRK")
WHOkey[w,"country"] <- "Dem. People's Republic of Korea" 

pop.totals <- merge(WHOkey, pop.totals2, by = "country")
#CHECK
#for(i in 1:138){w <- which(pop.totals$iso3 == cni[i]); if(length(w)<1){print(c(cni[i]))}}

## Age categories
pop.totals$age_cat <- 17 # if not in younger then must be in older
pop.totals[which(pop.totals$age == "0-4"),"age_cat"] <- 1
pop.totals[which(pop.totals$age == "5-9"),"age_cat"] <- 2
pop.totals[which(pop.totals$age == "10-14"),"age_cat"] <- 3
pop.totals[which(pop.totals$age == "15-19"),"age_cat"] <- 4
pop.totals[which(pop.totals$age == "20-24"),"age_cat"] <- 5
pop.totals[which(pop.totals$age == "25-29"),"age_cat"] <- 6
pop.totals[which(pop.totals$age == "30-34"),"age_cat"] <- 7
pop.totals[which(pop.totals$age == "35-39"),"age_cat"] <- 8
pop.totals[which(pop.totals$age == "40-44"),"age_cat"] <- 9
pop.totals[which(pop.totals$age == "45-49"),"age_cat"] <- 10
pop.totals[which(pop.totals$age == "50-54"),"age_cat"] <- 11
pop.totals[which(pop.totals$age == "55-59"),"age_cat"] <- 12
pop.totals[which(pop.totals$age == "60-64"),"age_cat"] <- 13
pop.totals[which(pop.totals$age == "65-69"),"age_cat"] <- 14
pop.totals[which(pop.totals$age == "70-74"),"age_cat"] <- 15
pop.totals[which(pop.totals$age == "75-79"),"age_cat"] <- 16

## years
years_n <- c(seq(1975,2010,5),2014)

ppg <- c()

for(i in 1:length(years_n)){
  s_level <- c() # store all levels
  print(c("year",years_n[i]))
  # move to where data is for this year
  setwd(paste0("~/Dropbox/MDR/output/Out/out_",years_n[i],"/"))
  
  if(years_n[i] < 2012){
    pop.totals.yr <- pop.totals[which(pop.totals$year == years_n[i]),]}
  if(years_n[i] == 2014){
    pop.totals.yr <- POP2014
    pop.totals.yr$age_cat <- seq(1,17,1)
    pop.totals.yr$popsize <- pop.totals.yr$value
  }
  
  for(j in 1:length(cni)){
    
    country <- cni[j] # country
    
    print(c("country",j, country))  
    
    # DATA
    ss <- read.csv(paste0(country,"_200_infor_prior.csv"))[,-1] # missing first row for some reason?
    colnames(ss) <- c("rep","iso3","pr_ds","pr_dr","new_ds","new_dr","rei_sr","rei_rs","year","age")  
    ss <- rbind(ss[1,],ss) # missing first row for some reason? 
    ss[1,"age"] <- 1
    
    # pop size
    pop <- pop.totals.yr[which(pop.totals.yr$iso3 == country),]
    
    w<-which(pop$age_cat == 17)
    pop[w,"popsize"] <- sum(pop[w,"popsize"])
    
    pop <- pop[order(pop$age_cat),][1:17,"popsize"]
    
    #Number in each
    # group by 5 yr to 80+ 
    # ds_age <- c();dr_age <- c()
    for(ii in 1:200){
      
      c_ds_age <- colwise(mean)(as.data.frame(matrix(ss[which(ss$rep == ii),"pr_ds"], 5)))
      ds_age <- as.numeric(c(c_ds_age[1:16], mean(as.numeric(c_ds_age[17:20]))))
      c_dr_age <- colwise(mean)(as.data.frame(matrix(ss[which(ss$rep == ii),"pr_dr"], 5)))
      dr_age <- as.numeric(c(c_dr_age[1:16], mean(as.numeric(c_dr_age[17:20]))))
      
      # size of the population infected
      size_ds <- pop * ds_age
      size_dr <- pop * dr_age
      
      # percentage of the population
      perc_ds <- 100*size_ds / sum(pop, na.rm = TRUE)
      perc_dr <- 100*size_dr / sum(pop, na.rm = TRUE)
      
      # total percentage infected sums
      ltbi_dr <- sum(perc_dr, na.rm = TRUE) # percentage infected
      ltbi_ds <- sum(perc_ds, na.rm = TRUE)
      pltbi_dr <- sum(size_dr, na.rm = TRUE) # number of people infected
      pltbi_ds <- sum(size_ds, na.rm = TRUE)
      
      ltbi_dr_kids <- sum(perc_dr[1:3], na.rm = TRUE) # percentage infected
      ltbi_ds_kids <- sum(perc_ds[1:3], na.rm = TRUE)
      pltbi_dr_kids <- sum(size_dr[1:3], na.rm = TRUE) # number of people infected
      pltbi_ds_kids <- sum(size_ds[1:3], na.rm = TRUE)
      
      # Bind together.
      s_level <- rbind(s_level,c(years_n[i],j, ii, 
                                 ltbi_dr,ltbi_ds,pltbi_dr, pltbi_ds,
                                 ltbi_dr_kids,ltbi_ds_kids, pltbi_dr_kids, pltbi_ds_kids, 
                                 sum(pop, na.rm = TRUE), sum(pop[1:3], na.rm = TRUE)))
      
    }
  }
  
  # sort out
  s_level <- as.data.frame(s_level)
  colnames(s_level) <-c("year","country","rep","ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids","pop","pop_kids")
  s_level$iso3 <- cni[s_level$country]
  
  # save it
  write.csv(s_level, paste0("~/Dropbox/MDR/output/s_level_200_", years_n[i],".csv"))
  
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
  
  ### Risk ratio
  ltbi_global$rr <- (ltbi_global$pltbir_kids / (ltbi_global$pltbir_kids + ltbi_global$pltbis_kids) ) / (ltbi_global$pltbir / (ltbi_global$pltbir + ltbi_global$pltbis))
  
  rr.med.g <- aggregate(ltbi_global[,"rr"],list(ltbi_global$Group.1),median)
  rr.ub.g <- aggregate(ltbi_global[,"rr"],list(ltbi_global$Group.1),ub)
  rr.lb.g <- aggregate(ltbi_global[,"rr"],list(ltbi_global$Group.1),lb)
  
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
  
  ### Risk ratio: global
  ltbi_total$rr <- (ltbi_total$pltbir_kids / (ltbi_total$pltbir_kids + ltbi_total$pltbis_kids) ) / (ltbi_total$pltbir / (ltbi_total$pltbir + ltbi_total$pltbis))
  
  paste0(round(median(ltbi_total$rr),2), " [",round(lb(ltbi_total$rr),2),"-",round(ub(ltbi_total$rr),2),"]")
  
  
  
  rr.med.g <- aggregate(ltbi_total[,"rr"],list(ltbi_total$Group.1),median)
  rr.ub.g <- aggregate(ltbi_total[,"rr"],list(ltbi_total$Group.1),ub)
  rr.lb.g <- aggregate(ltbi_total[,"rr"],list(ltbi_total$Group.1),lb)
  
  
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
  
  ##Global: NUMBER
  table1_global_numb <- as.data.frame(cbind( as.character(med.ltbir.g$Group.1),
                                             paste0(prettyNum(signif(med.ltbir.g$pltbis,3),big.mark=","), " [", 
                                                    prettyNum(signif(lb.ltbir.g$pltbis,3),big.mark=","), "-", 
                                                    prettyNum(signif(ub.ltbir.g$pltbis,3),big.mark=","),"]"),
                                             paste0(prettyNum(signif(med.ltbir.g$pltbir,3),big.mark=","), " [", 
                                                    prettyNum(signif(lb.ltbir.g$pltbir,3),big.mark=","), "-", 
                                                    prettyNum(signif(ub.ltbir.g$pltbir,3),big.mark=","),"]")
  ))
  ## Total
  total_numb <- as.data.frame(cbind("GLOBAL",
                                    paste0(prettyNum(signif(med.total$pltbis,3),big.mark=","), " [", 
                                           prettyNum(signif(lb.total$pltbis,3),big.mark=","), "-", 
                                           prettyNum(signif(ub.total$pltbis,3),big.mark=","),"]"),
                                    paste0(prettyNum(signif(med.total$pltbir,3),big.mark=","), " [", 
                                           prettyNum(signif(lb.total$pltbir,3),big.mark=","), "-", 
                                           prettyNum(signif(ub.total$pltbir,3),big.mark=","),"]")
  ))
  
  table1_global_numb <- rbind(table1_global_numb, total_numb)
  
  colnames(table1_global_numb) <- c("WHO Region","# with LTBIS", "# with LTBIR")
  table1_global_numb <- table1_global_numb[c(1,2,5,3,6,4,7),]
  
  ## Store
  
  write.csv(table1_countries, paste0("~/Dropbox/MDR/paper_output/table1_countries_",labl,"_",years_n[i],".csv"))
  write.csv(table1_countries_numb, paste0("~/Dropbox/MDR/paper_output/table1_countries_number_",labl,"_",years_n[i],".csv"))
  write.csv(table1_global, paste0("~/Dropbox/MDR/paper_output/table1_global_",labl,"_",years_n[i],".csv"))
  write.csv(table1_global_numb, paste0("~/Dropbox/MDR/paper_output/table1_global_numb_",labl,"_",years_n[i],".csv"))
  
  ### Plot these levels
  plot.g <- merge(med.ltbir.g[,c("Group.1","ltbir")], ub.ltbir.g[,c("Group.1","ltbir")], by = "Group.1")
  plot.g <- merge(plot.g, lb.ltbir.g[,c("Group.1","ltbir")], by = "Group.1")
  ggplot(plot.g, aes(x=Group.1, y = ltbir.x)) + geom_point() + 
    geom_errorbar(aes(x=Group.1, ymin = ltbir, ymax = ltbir.y)) + scale_y_continuous("MDR-LTBI prevalence") + scale_x_discrete("WHO region")
  ggsave(paste0("~/Dropbox/MDR/output/global_levels_ltbir_",pp,"_",years_n[i],".pdf"),width=13, height=8)
  
  # store this for the plot
  ppg <- rbind(ppg, cbind(plot.g,years_n[i]))
  
}

### All data
ppg0 <- as.data.frame(ppg)
colnames(ppg0) <- c("region","med","max","min","year")
write.csv(ppg0, "~/Dropbox/MDR/output/ltbi_over_time.csv")

ggplot(ppg0, aes(x=year, y = med, group = region)) + geom_point() + facet_wrap(~region) + 
  geom_errorbar(aes(x=year, ymin = min, ymax = max)) + scale_y_continuous("MDR-LTBI prevalence") + scale_x_continuous("Year")
ggsave(paste0("~/Dropbox/MDR/output/region_levels_ltbir_over_time.pdf"),width=13, height=8)

ggplot(ppg0, aes(x=year)) + geom_line(aes(y = med)) + 
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, fill = "red") +
  facet_wrap(~region) + 
  scale_y_continuous("MDR-LTBI prevalence (%)") + scale_x_continuous("Year")
ggsave(paste0("~/Dropbox/MDR/output/region_levels_ltbir_over_time_ribbon.pdf"),width=13, height=8)
