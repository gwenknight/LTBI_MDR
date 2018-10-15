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
  if(i == 1){recinf_all <- recinf}
  recinf_all <- rbind(recinf_all, recinf)
}

recinf_all <- as.data.frame(recinf_all)
rr <- recinf_all %>% group_by(pop_name, mdr_rep) 








  s$kids <- 0
  s[which(s$age < 15),"kids"] <- 1
  
  # aggregate by kids
  s_k <- s %>% group_by(year,cn,mdr_rep,kids) %>% 
    summarise_at(c("pr_dr","pr_ds","new_ds","new_dr","rei_sr","rei_rs"),funs(sum))
  # aggregate by all
  s_a <- s %>% group_by(year,cn,mdr_rep) %>% 
    summarise_at(c("pr_dr","pr_ds","new_ds","new_dr","rei_sr","rei_rs"),funs(sum))
  
  
  # store
  if(i == 1){s_all <- s_a; s_kids <- s_k; s_orig <- s
  }else{
    s_all <- rbind(s_all, s_a)
    s_kids <- rbind(s_kids, s_k)
    s_orig <- rbind(s_orig,s)}
  
}



s_all <- as.data.frame(s_all)
colnames(s_all) <- colnames((s_a))

s_kids <- as.data.frame(s_kids)
colnames(s_kids) <- colnames((s_k))

#s
s_all$sum_new_s <- (as.numeric(s_all$new_ds) + as.numeric(s_all$rei_rs))
s_kids$sum_new_s <- (as.numeric(s_kids$new_ds) + as.numeric(s_kids$rei_rs))

s_all$prop_new_s <- (as.numeric(s_all$new_ds) + as.numeric(s_all$rei_rs))/as.numeric(s_all$pr_ds)
s_kids$prop_new_s <- (as.numeric(s_kids$new_ds) + as.numeric(s_kids$rei_rs))/as.numeric(s_kids$pr_ds)

#r
s_all$sum_new_r <- (as.numeric(s_all$new_dr) + as.numeric(s_all$rei_sr))
s_kids$sum_new_r <- (as.numeric(s_kids$new_dr) + as.numeric(s_kids$rei_sr))

s_all$prop_new_r <- (as.numeric(s_all$new_dr) + as.numeric(s_all$rei_sr))/as.numeric(s_all$pr_dr)
s_kids$prop_new_r <- (as.numeric(s_kids$new_dr) + as.numeric(s_kids$rei_sr))/as.numeric(s_kids$pr_dr)

s_allm <- s_all %>% group_by(cn, mdr_rep) %>% summarise_at(c("sum_new_s","sum_new_r"), funs(sum))

# functions
ub <- function(x)quantile(x,probs = .975, na.rm = TRUE)
lb <- function(x)quantile(x,probs = .025, na.rm = TRUE)

med.s_allm <- aggregate(s_allm[,c("sum_new_s","sum_new_r")],list(s_allm$cn), median, na.rm = TRUE)
lb.s_allm <- aggregate(s_allm[,c("sum_new_s","sum_new_r")],list(s_allm$cn), lb)
ub.s_allm <- aggregate(s_allm[,c("sum_new_s","sum_new_r")],list(s_allm$cn), ub)

## WHO levels
load('~/Documents/LTBI_MDR/data/whokey.Rdata') # WHOkey has global region and iso3 codes
s_allm$iso3 <- s_allm$cn
s_allm.g <- merge(s_allm,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)
med.s_allm.g <- aggregate(s_allm.g[,c("sum_new_s","sum_new_r")],list(s_allm.g$g_whoregion), median, na.rm = TRUE)
lb.s_allm.g <- aggregate(s_allm.g[,c("sum_new_s","sum_new_r")],list(s_allm.g$g_whoregion), lb)
ub.s_allm.g <- aggregate(s_allm.g[,c("sum_new_s","sum_new_r")],list(s_allm.g$g_whoregion), ub)


s_allm_total <-aggregate(s_allm.g[,c("sum_new_s","sum_new_r")],list(s_allm.g$mdr_rep), sum)
med.s_all_m.total<-colwise(median)(s_allm_total[,c("sum_new_s","sum_new_r")])
lb.s_all_m.total<-colwise(lb)(s_allm_total[,c("sum_new_s","sum_new_r")])
ub.s_all_m.total<-colwise(ub)(s_allm_total[,c("sum_new_s","sum_new_r")])
