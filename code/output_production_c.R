### Combine results to give output

# Load in required data
load('../data/whokey.Rdata') # WHOkey has global region and iso3 codes


for(ii in 1:3){# Three models
  
  # Labels for each model
  if(ii == 1){labl = "lin"}
  if(ii == 2){labl = "quad"}
  if(ii == 3){labl = "quadc"}
  
  # number of replicates (up to 1000)
  nari <- 5
  
  # level at 2014
  s_level <- read.csv(paste0("../output/s_level_",nari,"_",labl,".csv"))[,-1]
  s_level$iso3 <- s_level$pop_name
  
  # Kid levels
  s_level$perc_ds_kids <- 100*s_level$pltbis_kids / s_level$pltbis
  s_level$perc_dr_kids <- 100*s_level$pltbir_kids / s_level$pltbir
  
  #### By country
  med.ltbir <- aggregate(ltbi[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids","perc_ds_kids","perc_dr_kids")], 
            list(ltbi$iso3), median)

  ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
  lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)
  
  ub.ltbir <- aggregate(ltbi[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids","perc_ds_kids","perc_dr_kids")], 
                         list(ltbi$iso3), ub)
  
  lb.ltbir <- aggregate(ltbi[,c("ltbir","ltbis","pltbir","pltbis","ltbir_kids","ltbis_kids","pltbir_kids","pltbis_kids","perc_ds_kids","perc_dr_kids")], 
                         list(ltbi$iso3), lb)
  
  med.ltbir$iso3 <- med.ltbir$Group.1
  ub.ltbir$iso3 <- ub.ltbir$Group.1
  lb.ltbir$iso3 <- lb.ltbir$Group.1

  # Add in global regions
  ltbi <- merge(s_level,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
  
  #### By global region
  ltbi_global <- aggregate(ltbi[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")], 
                         list(ltbi$g_whoregion,ltbi$rep), sum)
  
  ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
  lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)
  
  med.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")],
                           list(ltbi_global$Group.1),median)
  
  ub.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")],
                           list(ltbi_global$Group.1),ub)
  
  lb.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")],
                           list(ltbi_global$Group.1),lb)
  
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
  
  
  
    
}
