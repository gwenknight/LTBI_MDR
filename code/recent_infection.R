### Recent infection 

## Read in 2013 & 2014 levels
## 2012 - 2014 
cni <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
length(cni) # 138 now
llu <- length(cni)

load('~/Documents/LTBI_MDR/data/POP2014.Rdata')  

for(i in 1:llu){
  print(i)
  s <- read.csv(paste0("~/Dropbox/MDR/output/",cni[i],"_rec_infec_",nari,"_infor_prior.csv"))[,-1]
  
  ### Age groups
  s$new_inf_s <- s$new_ds + s$rei_rs
  s$new_inf_r <- s$new_dr + s$rei_sr

  c_ds_age <- colMeans(matrix(s[,"new_inf_s"],ncol = 40000/5))
  new_ds_age <- c()
  for (ij in 1:400){
    new_ds_age <- c(new_ds_age, 
                    as.numeric(c(c_ds_age[((ij-1)*20 + 1):((ij-1)*20+16)], 
                               mean(as.numeric(c_ds_age[((ij-1)*20+17):((ij-1)*20+20)])))))
  }
  c_dr_age <- colMeans(matrix(s[,"new_inf_r"],ncol = 40000/5))
  new_dr_age <- c()
  for (ij in 1:400){
    new_dr_age <- c(new_dr_age, 
                    as.numeric(c(c_dr_age[((ij-1)*20 + 1):((ij-1)*20+16)], 
                                 mean(as.numeric(c_dr_age[((ij-1)*20+17):((ij-1)*20+20)])))))
  }
  
  # population size
  pop <- as.data.frame(POP2014[which(as.character(POP2014$iso3) == as.character(cni[i])),"value"])
  
  if(length(new_dr_age) != 6800){print(c(i,length(new_dr_age)))}
  # MDR REP
  recinf <- as.data.frame(cbind(new_dr_age, new_ds_age))
  recinf$age_cat <- rep(seq(1,17,1), 400)
  recinf$mdr_rep <- rep(seq(1,200,1), each = 34)
  recinf$pop <- rep(as.numeric(pop$value), 400)
  recinf$year <- rep(rep(c(2013,2014), each =17), 200)
    
  # size of the population infected
  recinf$size_ds <- recinf$pop * recinf$new_ds_age
  recinf$size_dr <- recinf$pop * recinf$new_dr_age
  
  # labels
  recinf$total_pop <- sum(pop)
  recinf$pop_name <- cni[i]
  
  ## store
  if(i == 1){recinf_all <- recinf}else{
  recinf_all <- rbind(recinf_all, recinf)}
}

# dim(recinf_all) # 138*200*2*17
recinf_all <- as.data.frame(recinf_all)
rr <- recinf_all %>% group_by(pop_name, mdr_rep,total_pop) %>% summarise_at(c("size_dr","size_ds"),funs(sum))

# functions
ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)

med.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), median, na.rm = TRUE)
lb.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), lb)
ub.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), ub)

## WHO levels
load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes
rr$iso3 <- rr$pop_name
rr.g <- merge(rr,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
med.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), median, na.rm = TRUE)
lb.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), lb)
ub.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), ub)

## Total
rr_total <-aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$mdr_rep), sum)
med.rr.total<-colwise(median)(rr_total[,c("size_ds","size_dr")])
lb.rr.total<-colwise(lb)(rr_total[,c("size_ds","size_dr")])
ub.rr.total<-colwise(ub)(rr_total[,c("size_ds","size_dr")])


### Percentage of global burden with recent LTBI
global_pop <- sum(rr$total_pop) / 2 / 100 # 6893893 thousand # of these 138 countries 


# ALL 
paste0(sprintf('%.1f',100 * sum(med.rr.total) / global_pop), " [", 
       sprintf('%.1f',100 * sum(lb.rr.total) / global_pop), "-", 
       sprintf('%.1f',100 * sum(ub.rr.total) / global_pop),"]")

# DS
paste0(sprintf('%.1f',100 * med.rr.total$size_ds / global_pop), " [", 
       sprintf('%.1f',100 * lb.rr.total$size_ds / global_pop), "-", 
       sprintf('%.1f',100 * ub.rr.total$size_ds / global_pop),"]")
# MDR
paste0(sprintf('%.2f',100 * med.rr.total$size_dr / global_pop), " [", 
       sprintf('%.2f',100 * lb.rr.total$size_dr / global_pop), "-", 
       sprintf('%.2f',100 * ub.rr.total$size_dr / global_pop),"]")

paste0(signif(med.rr.total$size_dr,2), " [", 
       signif(lb.rr.total$size_dr,2), "-", 
               signif(ub.rr.total$size_dr,2),"]")


## Percentage MDR
100*med.rr.total$size_dr / (med.rr.total$size_ds + med.rr.total$size_dr)

####**** KIDS ****############
recinf_all_kids <- recinf_all[which(recinf_all$age_cat < 4),]
rr <- recinf_all_kids %>% group_by(pop_name, mdr_rep,total_pop) %>% summarise_at(c("size_dr","size_ds"),funs(sum))

# dim(recinf_all_kids) # 138*3*2*200
global_pop_kids <- sum(recinf_all_kids$pop) / (2*200)  # ~ 25% of population are < 15yo

med.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), median, na.rm = TRUE)
lb.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), lb)
ub.rr <- aggregate(rr[,c("size_ds","size_dr")],list(rr$pop_name), ub)

## WHO levels
load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes
rr$iso3 <- rr$pop_name
rr.g <- merge(rr,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
med.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), median, na.rm = TRUE)
lb.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), lb)
ub.rr.g <- aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$g_whoregion), ub)

## Total
rr_total <-aggregate(rr.g[,c("size_ds","size_dr")],list(rr.g$mdr_rep), sum)
rr_total$perc_r <- 100*rr_total$size_dr / (rr_total$size_dr + rr_total$size_ds)

med.rr.total<-colwise(median)(rr_total[,c("size_ds","size_dr","perc_r")])
lb.rr.total<-colwise(lb)(rr_total[,c("size_ds","size_dr","perc_r")])
ub.rr.total<-colwise(ub)(rr_total[,c("size_ds","size_dr","perc_r")])


### Percentage of global burden with recent LTBI
global_pop <- sum(rr$total_pop) / 2 / 100 # 6893893 thousand # of these 138 countries 


# ALL 
paste0(sprintf('%.1f',100 * sum(med.rr.total) / global_pop), " [", 
       sprintf('%.1f',100 * sum(lb.rr.total) / global_pop), "-", 
       sprintf('%.1f',100 * sum(ub.rr.total) / global_pop),"]")

# DS
paste0(sprintf('%.1f',100 * med.rr.total$size_ds / global_pop), " [", 
       sprintf('%.1f',100 * lb.rr.total$size_ds / global_pop), "-", 
       sprintf('%.1f',100 * ub.rr.total$size_ds / global_pop),"]")
# MDR
paste0(sprintf('%.2f',100 * med.rr.total$size_dr / global_pop), " [", 
       sprintf('%.2f',100 * lb.rr.total$size_dr / global_pop), "-", 
       sprintf('%.2f',100 * ub.rr.total$size_dr / global_pop),"]")

paste0(signif(med.rr.total$size_dr,2), " [", 
       signif(lb.rr.total$size_dr,2), "-", 
       signif(ub.rr.total$size_dr,2),"]")


## Percentage MDR
100*med.rr.total$size_dr / (med.rr.total$size_ds + med.rr.total$size_dr)

