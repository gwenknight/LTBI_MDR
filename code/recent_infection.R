### Recent infection 
library(plyr)
library(magrittr)
library(dplyr)

## Read in 2013 & 2014 levels
## 2012 - 2014 
cni <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
length(cni) # 138 now
llu <- 5#length(cni)
nari = 200

# functions
ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)

load('~/Documents/LTBI_MDR/data/POP2014.Rdata')  
recinf <- c()

for(i in 1:llu){
  print(i)
  s <- read.csv(paste0("~/Dropbox/MDR/output/",cni[i],"_rec_infec_",nari,"_infor_prior.csv"))[,-1]
  
  ### Age groups
  s$new_inf_s <- s$new_ds + s$rei_rs
  s$new_inf_r <- s$new_dr + s$rei_sr
  
  s$prop_new_inf_r <- s$new_inf_r / (s$new_inf_r + s$new_inf_s)
  w<-which(s$age < 15)
  s$prop_new_kids <- 0
  s[w,"prop_new_kids"] <- s[w,"new_inf_r"] / (s[w,"new_inf_r"] + s[w,"new_inf_s"])
  
  # population size
  pop <- as.data.frame(POP2014[which(as.character(POP2014$iso3) == as.character(cni[i])),"value"])
  
  for(jj in 2013:2014){
    # for each year
    sn <- s[which(s$year == jj),]
    
    for(ii in 1:200){
      c_ds_age <- colwise(mean)(as.data.frame(matrix(sn[which(sn$mdr_rep == ii),"new_inf_s"], 5)))
      ds_age <- as.numeric(c(c_ds_age[1:16], mean(as.numeric(c_ds_age[17:20]))))
      c_dr_age <- colwise(mean)(as.data.frame(matrix(sn[which(sn$mdr_rep == ii),"new_inf_r"], 5)))
      dr_age <- as.numeric(c(c_dr_age[1:16], mean(as.numeric(c_dr_age[17:20]))))
      
      c_ds_age1 <- colwise(mean)(as.data.frame(matrix(sn[which(sn$mdr_rep == ii),"new_ds"], 5)))
      ds_age1 <- as.numeric(c(c_ds_age1[1:16], mean(as.numeric(c_ds_age1[17:20]))))
      c_dr_age1 <- colwise(mean)(as.data.frame(matrix(sn[which(sn$mdr_rep == ii),"new_dr"], 5)))
      dr_age1 <- as.numeric(c(c_dr_age1[1:16], mean(as.numeric(c_dr_age1[17:20]))))
      
      c_ds_age2 <- colwise(mean)(as.data.frame(matrix(sn[which(sn$mdr_rep == ii),"rei_sr"], 5)))
      ds_age2 <- as.numeric(c(c_ds_age2[1:16], mean(as.numeric(c_ds_age2[17:20]))))
      c_dr_age2 <- colwise(mean)(as.data.frame(matrix(sn[which(sn$mdr_rep == ii),"rei_rs"], 5)))
      dr_age2 <- as.numeric(c(c_dr_age2[1:16], mean(as.numeric(c_dr_age2[17:20]))))
      
      # size of the population infected
      size_ds <- pop * ds_age
      size_dr <- pop * dr_age
      size_ds_n <- pop * ds_age1
      size_dr_n <- pop * dr_age1
      size_ds_re <- pop * ds_age2
      size_dr_re <- pop * dr_age2
      
      # percentage of the population
      #perc_ds <- 100*size_ds / sum(pop, na.rm = TRUE)
      #perc_dr <- 100*size_dr / sum(pop, na.rm = TRUE)
      
      # total percentage infected sums
      #ltbi_dr <- sum(perc_dr, na.rm = TRUE) # percentage infected
      #ltbi_ds <- sum(perc_ds, na.rm = TRUE)
      pltbi_dr <- sum(size_dr, na.rm = TRUE) # number of people infected
      pltbi_ds <- sum(size_ds, na.rm = TRUE)
      pltbi_dr_n <- sum(size_dr_n, na.rm = TRUE) # number of people infected
      pltbi_ds_n <- sum(size_ds_n, na.rm = TRUE)
      pltbi_dr_re <- sum(size_dr_re, na.rm = TRUE) # number of people infected
      pltbi_ds_re <- sum(size_ds_re, na.rm = TRUE)
      
      #ltbi_dr_kids <- sum(perc_dr[1:3,1], na.rm = TRUE) # percentage infected
      #ltbi_ds_kids <- sum(perc_ds[1:3,1], na.rm = TRUE)
      pltbi_dr_kids <- sum(size_dr[1:3,1], na.rm = TRUE) # number of people infected
      pltbi_ds_kids <- sum(size_ds[1:3,1], na.rm = TRUE)
      pltbi_dr_kids_n <- sum(size_dr_n[1:3,1], na.rm = TRUE) # number of people infected
      pltbi_ds_kids_n <- sum(size_ds_n[1:3,1], na.rm = TRUE)
      pltbi_dr_kids_re <- sum(size_dr_re[1:3,1], na.rm = TRUE) # number of people infected
      pltbi_ds_kids_re <- sum(size_ds_re[1:3,1], na.rm = TRUE)
      
      # Bind together
      ## store
      
      recinf <- rbind(recinf,c(jj, i, ii, 
                               #ltbi_dr,ltbi_ds,
                               pltbi_dr, pltbi_ds,
                               #ltbi_dr_kids,ltbi_ds_kids, 
                               pltbi_dr_kids, pltbi_ds_kids, 
                               sum(pop, na.rm = TRUE), sum(pop[1:3,1], na.rm = TRUE),
                               pltbi_dr_n, pltbi_ds_n,
                               pltbi_dr_kids_n, pltbi_ds_kids_n, 
                               pltbi_dr_re, pltbi_ds_re,
                               pltbi_dr_kids_re, pltbi_ds_kids_re))
      
    }
  }
}

recinf <- as.data.frame(recinf)
colnames(recinf) <- c("year","cn","mdr_rep",#"ltbir","ltbis",
                      "pltbir","pltbis",
                      #"ltbir_kids","ltbis_kids",
                      "pltbir_kids","pltbis_kids",
                      "pop","pop_kids",
                      "pltbi_dr_n", "pltbi_ds_n",
                      "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                      "pltbi_dr_re", "pltbi_ds_re",
                      "pltbi_dr_kids_re", "pltbi_ds_kids_re")
recinf$iso3 <- cni[recinf$cn] 

# sum over years. Population size assumed same for both years. 
rr <- recinf %>% group_by(iso3, mdr_rep,pop,pop_kids) %>% 
  summarise_at(c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                 "pltbi_dr_n", "pltbi_ds_n",
                 "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                 "pltbi_dr_re", "pltbi_ds_re",
                 "pltbi_dr_kids_re", "pltbi_ds_kids_re"),funs(sum))

# country levels
med.rr <- aggregate(rr[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                          "pltbi_dr_n", "pltbi_ds_n",
                          "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                          "pltbi_dr_re", "pltbi_ds_re",
                          "pltbi_dr_kids_re", "pltbi_ds_kids_re")],
                    list(rr$iso3), median, na.rm = TRUE)
lb.rr <- aggregate(rr[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                         "pltbi_dr_n", "pltbi_ds_n",
                         "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                         "pltbi_dr_re", "pltbi_ds_re",
                         "pltbi_dr_kids_re", "pltbi_ds_kids_re")],list(rr$iso3), lb)
ub.rr <- aggregate(rr[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                         "pltbi_dr_n", "pltbi_ds_n",
                         "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                         "pltbi_dr_re", "pltbi_ds_re",
                         "pltbi_dr_kids_re", "pltbi_ds_kids_re")],list(rr$iso3), ub)

## WHO levels
load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes
rr.g <- merge(rr,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
# sum up country level to g_whoregon by mdr_rep
rr.g <- rr.g %>% group_by(g_whoregion, mdr_rep) %>% 
  summarise_at(c("pop","pop_kids","pltbir","pltbis","pltbir_kids","pltbis_kids",
                 "pltbi_dr_n", "pltbi_ds_n",
                 "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                 "pltbi_dr_re", "pltbi_ds_re",
                 "pltbi_dr_kids_re", "pltbi_ds_kids_re"),funs(sum))

# region levels
med.rr.g <- aggregate(rr.g[,c("pop","pop_kids","pltbir","pltbis","pltbir_kids","pltbis_kids",
                              "pltbi_dr_n", "pltbi_ds_n",
                              "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                              "pltbi_dr_re", "pltbi_ds_re",
                              "pltbi_dr_kids_re", "pltbi_ds_kids_re")],
                      list(rr.g$g_whoregion), median, na.rm = TRUE)
lb.rr.g <- aggregate(rr.g[,c("pop","pop_kids","pltbir","pltbis","pltbir_kids","pltbis_kids",
                             "pltbi_dr_n", "pltbi_ds_n",
                             "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                             "pltbi_dr_re", "pltbi_ds_re",
                             "pltbi_dr_kids_re", "pltbi_ds_kids_re")],list(rr.g$g_whoregion), lb)
ub.rr.g <- aggregate(rr.g[,c("pop","pop_kids","pltbir","pltbis","pltbir_kids","pltbis_kids",
                             "pltbi_dr_n", "pltbi_ds_n",
                             "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                             "pltbi_dr_re", "pltbi_ds_re",
                             "pltbi_dr_kids_re", "pltbi_ds_kids_re")],list(rr.g$g_whoregion), ub)

## Total
rr_total <-aggregate(rr.g[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                             "pltbi_dr_n", "pltbi_ds_n",
                             "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                             "pltbi_dr_re", "pltbi_ds_re",
                             "pltbi_dr_kids_re", "pltbi_ds_kids_re")],list(rr.g$mdr_rep), sum)
med.rr.total<-colwise(median)(rr_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                          "pltbi_dr_n", "pltbi_ds_n",
                                          "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                                          "pltbi_dr_re", "pltbi_ds_re",
                                          "pltbi_dr_kids_re", "pltbi_ds_kids_re")])
lb.rr.total<-colwise(lb)(rr_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                     "pltbi_dr_n", "pltbi_ds_n",
                                     "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                                     "pltbi_dr_re", "pltbi_ds_re",
                                     "pltbi_dr_kids_re", "pltbi_ds_kids_re")])
ub.rr.total<-colwise(ub)(rr_total[,c("pltbir","pltbis","pltbir_kids","pltbis_kids",
                                     "pltbi_dr_n", "pltbi_ds_n",
                                     "pltbi_dr_kids_n", "pltbi_ds_kids_n", 
                                     "pltbi_dr_re", "pltbi_ds_re",
                                     "pltbi_dr_kids_re", "pltbi_ds_kids_re")])

### Percentage of global burden with recent LTBI
global_pop <- sum(rr$pop)/200 # 6893893 thousand # makes sense
global_pop_kids <- sum(rr$pop_kids) / 200 # 1750707 
# 100*global_pop_kids / global_pop # 25% = makes sense

# ALL 
# global percentage with MDR
paste0(sprintf('%.3f',100 * sum(med.rr.total$pltbir) / global_pop), " [", 
       sprintf('%.3f',100 * sum(lb.rr.total$pltbir) / global_pop), "-", 
       sprintf('%.3f',100 * sum(ub.rr.total$pltbir) / global_pop),"]")

# DS
paste0(sprintf('%.1f',100 * med.rr.total$pltbis / global_pop), " [", 
       sprintf('%.1f',100 * lb.rr.total$pltbis / global_pop), "-", 
       sprintf('%.1f',100 * ub.rr.total$pltbis / global_pop),"]")
# MDR
paste0(sprintf('%.3f',100 * med.rr.total$pltbir / global_pop), " [", 
       sprintf('%.3f',100 * lb.rr.total$pltbir / global_pop), "-", 
       sprintf('%.3f',100 * ub.rr.total$pltbir / global_pop),"]")

# DS kids
paste0(sprintf('%.1f',100 * med.rr.total$pltbis_kids / global_pop_kids), " [", 
       sprintf('%.1f',100 * lb.rr.total$pltbis_kids / global_pop_kids), "-", 
       sprintf('%.1f',100 * ub.rr.total$pltbis_kids / global_pop_kids),"]")

# MDR kids
paste0(sprintf('%.3f',100 * med.rr.total$pltbir_kids / global_pop_kids), " [", 
       sprintf('%.3f',100 * lb.rr.total$pltbir_kids / global_pop_kids), "-", 
       sprintf('%.3f',100 * ub.rr.total$pltbir_kids / global_pop_kids),"]")

paste0(signif(med.rr.total$pltbir_kids,2), " [", 
       signif(lb.rr.total$pltbir_kids,2), "-", 
       signif(ub.rr.total$pltbir_kids,2),"]")


### FOR PAPER
# NUMBER WITH ANY RECENT INFECTION GLOBALLY
paste0(signif(med.rr.total$pltbir + med.rr.total$pltbis,2), " [", 
       signif(lb.rr.total$pltbir + med.rr.total$pltbis,2), "-", 
       signif(ub.rr.total$pltbir + med.rr.total$pltbis,2),"]")

# NUMBER WITH MDR RECENT INFECTION GLOBALLY
paste0(signif(med.rr.total$pltbir,2), " [", 
       signif(lb.rr.total$pltbir,2), "-", 
       signif(ub.rr.total$pltbir,2),"]")
# PERCENTAGE GLOBAL POPULATION WITH RECENT MDR INFECTION
paste0(sprintf('%.2f',100 * med.rr.total$pltbir / global_pop), " [", 
       sprintf('%.2f',100 * lb.rr.total$pltbir / global_pop), "-", 
       sprintf('%.2f',100 * ub.rr.total$pltbir / global_pop),"]")

paste0(sprintf('%.2f',100 * med.rr.total$pltbir_n / global_pop), " [", 
       sprintf('%.2f',100 * lb.rr.total$pltbir_n / global_pop), "-", 
       sprintf('%.2f',100 * ub.rr.total$pltbir_n / global_pop),"]")



# PERCENTAGE OF RECENT INFECTIONS WITH MDR
paste0(sprintf('%.3f',100 * med.rr.total$pltbir / (med.rr.total$pltbir + med.rr.total$pltbis)), " [", 
       sprintf('%.3f',100 * lb.rr.total$pltbir  / (med.rr.total$pltbir + med.rr.total$pltbis)), "-", 
       sprintf('%.3f',100 * ub.rr.total$pltbir  / (med.rr.total$pltbir + med.rr.total$pltbis)),"]")

# PERCENTAGE OF RECENT INFECTIONS WITH MDR IN KIDS
paste0(sprintf('%.3f',100 * med.rr.total$pltbir_kids / (med.rr.total$pltbir_kids + med.rr.total$pltbis_kids)), " [", 
       sprintf('%.3f',100 * lb.rr.total$pltbir_kids  / (med.rr.total$pltbir_kids + med.rr.total$pltbis_kids)), "-", 
       sprintf('%.3f',100 * ub.rr.total$pltbir_kids  / (med.rr.total$pltbir_kids + med.rr.total$pltbis_kids)),"]")

# NUMBER WITH MDR RECENT INFECTION GLOBALLY
paste0(signif(med.rr.total$pltbir_kids,2), " [", 
       signif(lb.rr.total$pltbir_kids,2), "-", 
       signif(ub.rr.total$pltbir_kids,2),"]")













### OLD

# c_ds_age <- colMeans(matrix(s[,"new_inf_s"],ncol = 40000/5))
# new_ds_age <- c()
# for (ij in 1:400){
#   new_ds_age <- c(new_ds_age, 
#                   as.numeric(c(c_ds_age[((ij-1)*20 + 1):((ij-1)*20+16)], 
#                              mean(as.numeric(c_ds_age[((ij-1)*20+17):((ij-1)*20+20)])))))
# }
# c_dr_age <- colMeans(matrix(s[,"new_inf_r"],ncol = 40000/5))
# new_dr_age <- c()
# for (ij in 1:400){
#   new_dr_age <- c(new_dr_age, 
#                   as.numeric(c(c_dr_age[((ij-1)*20 + 1):((ij-1)*20+16)], 
#                                mean(as.numeric(c_dr_age[((ij-1)*20+17):((ij-1)*20+20)])))))
# }
# 
# # population size
# pop <- as.data.frame(POP2014[which(as.character(POP2014$iso3) == as.character(cni[i])),"value"])
# 
# if(length(new_dr_age) != 6800){print(c(i,length(new_dr_age)))}
# # MDR REP
# recinf <- as.data.frame(cbind(new_dr_age, new_ds_age))
# recinf$age_cat <- rep(seq(1,17,1), 400)
# recinf$mdr_rep <- rep(seq(1,200,1), each = 34)
# recinf$pop <- rep(as.numeric(pop$value), 400)
# recinf$year <- rep(rep(c(2013,2014), each =17), 200)
#   
# # size of the population infected
# recinf$size_ds <- recinf$pop * recinf$new_ds_age
# recinf$size_dr <- recinf$pop * recinf$new_dr_age
# 
# # labels
# recinf$total_pop <- sum(pop)
# recinf$pop_name <- cni[i]
# 
# ## store
# if(i == 1){recinf_all <- recinf}else{
# recinf_all <- rbind(recinf_all, recinf)}
#}

# dim(recinf_all) # 138*200*2*17
# recinf_all <- as.data.frame(recinf)
# rr <- recinf_all %>% group_by(iso3, mdr_rep,pop,pop_kids) %>% summarise_at(c("pltbir","pltbis","pltbir_kids","pltbis_kids"),funs(sum))
# 


# 
# 
# ## Percentage MDR
# 100*med.rr.total$pltbir / (med.rr.total$pltbir + med.rr.total$pltbis)
# 
# ####**** KIDS ****############
# recinf_all_kids <- recinf_all[which(recinf_all$age_cat < 4),]
# rr <- recinf_all_kids %>% group_by(pop_name, mdr_rep,total_pop) %>% summarise_at(c("size_dr","size_ds"),funs(sum))
# 
# # dim(recinf_all_kids) # 138*3*2*200
# global_pop_kids <- sum(recinf_all_kids$pop) / (2*200)  # ~ 25% of population are < 15yo
# 
# med.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), median, na.rm = TRUE)
# lb.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), lb)
# ub.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), ub)
# 
# ## WHO levels
# load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes
# rr$iso3 <- rr$pop_name
# rr.g <- merge(rr,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
# med.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), median, na.rm = TRUE)
# lb.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), lb)
# ub.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), ub)
# 
# ## Total
# rr_total <-aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$mdr_rep), sum)
# rr_total$perc_r <- 100*rr_total$size_dr / (rr_total$size_dr + rr_total$size_ds)
# 
# med.rr.total<-colwise(median)(rr_total[,c("size_ds","size_dr","perc_r")])
# lb.rr.total<-colwise(lb)(rr_total[,c("size_ds","size_dr","perc_r")])
# ub.rr.total<-colwise(ub)(rr_total[,c("size_ds","size_dr","perc_r")])
# 
# 
# ### Percentage of global burden with recent LTBI
# global_pop <- sum(rr$total_pop) / 2 / 100 # 6893893 thousand # of these 138 countries 
# 
# 
# # ALL 
# paste0(sprintf('%.1f',100 * sum(med.rr.total) / global_pop), " [", 
#        sprintf('%.1f',100 * sum(lb.rr.total) / global_pop), "-", 
#        sprintf('%.1f',100 * sum(ub.rr.total) / global_pop),"]")
# 
# # DS
# paste0(sprintf('%.1f',100 * med.rr.total$size_ds / global_pop), " [", 
#        sprintf('%.1f',100 * lb.rr.total$size_ds / global_pop), "-", 
#        sprintf('%.1f',100 * ub.rr.total$size_ds / global_pop),"]")
# # MDR
# paste0(sprintf('%.2f',100 * med.rr.total$size_dr / global_pop), " [", 
#        sprintf('%.2f',100 * lb.rr.total$size_dr / global_pop), "-", 
#        sprintf('%.2f',100 * ub.rr.total$size_dr / global_pop),"]")
# 
# paste0(signif(med.rr.total$size_dr,2), " [", 
#        signif(lb.rr.total$size_dr,2), "-", 
#        signif(ub.rr.total$size_dr,2),"]")
# 
# 
# ## Percentage MDR
# 100*med.rr.total$size_dr / (med.rr.total$size_ds + med.rr.total$size_dr)
# 
