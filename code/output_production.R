### Combine results to give output

# Load in required data
load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes


for(ii in 1:4){# Three models and best
  
  # Labels for each model
  if(ii == 1){labl = "lin"}
  if(ii == 2){labl = "quad"}
  if(ii == 3){labl = "quadc"}
  if(ii == 4){labl = "best"}
  
  # number of replicates (up to 1000)
  nari <- 5
  
  # functions
  ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
  lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)
  
  # level at 2014
  s_level <- read.csv(paste0("~/Dropbox/MDR/output/s_level_",nari,"_",labl,".csv"))[,-1]
  s_level$iso3 <- s_level$pop_name
  
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
  # Total global level
  ltbi_total <- aggregate(ltbi[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")], 
                          list(ltbi$rep), sum) # still got replicates in there
  
  med.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")],
                           list(ltbi_global$Group.1),median)
  
  ub.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")],
                           list(ltbi_global$Group.1),ub)
  
  lb.ltbir.g <- aggregate(ltbi_global[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")],
                           list(ltbi_global$Group.1),lb)
  
  med.total <- colwise(median)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")])
  lb.total <- colwise(lb)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")])
  ub.total <- colwise(ub)(ltbi_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids","pop","pop_kids")])
  
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
  
  med.total$ltbis_kids <- 100*med.total$pltbis_kids / med.total$pop_kids
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
                                 paste0(sprintf('%.2f',med.ltbir$perc_dr_kids), " [", 
                                        sprintf('%.2f',lb.ltbir$perc_dr_kids), "-", 
                                        sprintf('%.2f',ub.ltbir$perc_dr_kids),"]"),
                                 paste0(sprintf('%.2f',med.ltbir$ltbir_kids), " [", 
                                        sprintf('%.2f',lb.ltbir$ltbir_kids), "-", 
                                        sprintf('%.2f',ub.ltbir$ltbir_kids),"]"),
                                 paste0(sprintf('%.2f',med.ltbir$perc_ltbi_mdr), " [", 
                                        sprintf('%.2f',lb.ltbir$perc_ltbi_mdr), "-", 
                                        sprintf('%.2f',ub.ltbir$perc_ltbi_mdr),"]")))
  
  colnames(table1_countries) <- c("iso3","Perc. with LTBIS", "Perc. of LTBIS in <15yos","Perc. of <15yos with LTBIS",
                                  "Perc. with LTBIR", "Perc. of LTBIR in <15yos","Perc. of <15yos with LTBIR","Perc. of LTBI that is MDR")

  
  ## Global
  table1_global <- as.data.frame(cbind( as.character(med.ltbir.g$Group.1),
                                         paste0(sprintf('%.1f',med.ltbir.g$ltbis), " [", 
                                                  sprintf('%.1f',lb.ltbir.g$ltbis), "-", 
                                                  sprintf('%.1f',ub.ltbir.g$ltbis),"]"),
                                        paste0(sprintf('%.1f',med.ltbir.g$ltbis_kids), " [", 
                                               sprintf('%.1f',lb.ltbir.g$ltbis_kids), "-", 
                                               sprintf('%.1f',ub.ltbir.g$ltbis_kids),"]"),
                                           paste0(sprintf('%.2f',med.ltbir.g$ltbir), " [", 
                                                  sprintf('%.2f',lb.ltbir.g$ltbir), "-", 
                                                  sprintf('%.2f',ub.ltbir.g$ltbir),"]"),
                                        paste0(sprintf('%.2f',med.ltbir.g$ltbir_kids), " [", 
                                               sprintf('%.2f',lb.ltbir.g$ltbir_kids), "-", 
                                               sprintf('%.2f',ub.ltbir.g$ltbir_kids),"]"),
                                        paste0(sprintf('%.2f',med.ltbir.g$perc_ltbi_mdr), " [", 
                                               sprintf('%.2f',lb.ltbir.g$perc_ltbi_mdr), "-", 
                                               sprintf('%.2f',ub.ltbir.g$perc_ltbi_mdr),"]")
                                        ))
  ## Total
  total <- as.data.frame(cbind("GLOBAL",
                                paste0(sprintf('%.1f',med.total$ltbis), " [", 
                                       sprintf('%.1f',lb.total$ltbis), "-", 
                                       sprintf('%.1f',ub.total$ltbis),"]"),
                                paste0(sprintf('%.1f',med.total$ltbis_kids), " [", 
                                       sprintf('%.1f',lb.total$ltbis_kids), "-", 
                                       sprintf('%.1f',ub.total$ltbis_kids),"]"),
                                paste0(sprintf('%.2f',med.total$ltbir), " [", 
                                       sprintf('%.2f',lb.total$ltbir), "-", 
                                       sprintf('%.2f',ub.total$ltbir),"]"),
                                paste0(sprintf('%.2f',med.total$ltbir_kids), " [", 
                                       sprintf('%.2f',lb.total$ltbir_kids), "-", 
                                       sprintf('%.2f',ub.total$ltbir_kids),"]"),
                               paste0(sprintf('%.2f',med.total$perc_ltbi_mdr), " [", 
                                      sprintf('%.2f',lb.total$perc_ltbi_mdr), "-", 
                                      sprintf('%.2f',ub.total$perc_ltbi_mdr),"]")
  ))
  
  table1_global <- rbind(table1_global, total)
  
  colnames(table1_global) <- c("WHO Region","Perc. with LTBIS", "Perc. of <15yos with LTBIS",
                                  "Perc. with LTBIR", "Perc. of <15yos with LTBIR","Perc. of LTBI that is MDR")
  table1_global <- table1_global[c(1,2,5,3,6,3,7),]
  
  ## Store
  xtable(table1_countries)
  xtable(table1_global)
  
  write.csv(table1_countries, paste0("~/Dropbox/MDR/paper_output/table1_countries_",labl,".csv"))
  write.csv(table1_global, paste0("~/Dropbox/MDR/paper_output/table1_global_",labl,".csv"))

}
