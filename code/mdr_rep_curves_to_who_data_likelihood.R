##### Compare linear MDR ARI curves to data

### Libraries
library(ggplot2); library(dplyr); library(data.table)
theme_set(theme_bw(base_size=24))

### Read in trends
save_curves0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent_MDR/Data/lin_sig_quad_sigd_curves.csv",stringsAsFactors = FALSE)[-1]
#mdr_last0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/data/mdr_last.csv",stringsAsFactors = FALSE)[-1]

### Read in WHO data
who0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/new_who_edited_sub.csv",stringsAsFactors = FALSE)[-1]

## add in extra columns
who0$mdr_new <- who0$new_mdr_prop

## years after 2014...
#ww <- which(who$year > 2014)
#if(length(ww) > 0){who[ww,"year"] <- 2014}

## Countries to look at 
cn <- read.csv("~/Dropbox/MDR/107_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
length(unique(who0$iso3))
length(intersect(unique(who0$iso3), cn)) # 107? yep. 

## Where saving output? 
setwd("~/Dropbox/MDR/output/")

## Likelihood function
source("~/Documents/LTBI_MDR/code/likelihood_function.R")

## Matrix for data
store_cn_best <- c()
ll_all <- c()

## Only want linear
save_curves_lin <- subset(save_curves0, type == 1)
save_curves <- save_curves_lin

## For each country
for(i in 1:length(cn)){
  
  ## iso3 which country?
  cc <- cn[i]
  print(c(i,cc))
  
  ## data - for this country
  who <- who0[which(who0$iso3 == cc),c("year","mdr_new","mhi")]
  
  ## What data points past 2000? 
  #who$year0 <- who$year
  ww10 <- which(who$year > 2000)
  
  
  ## final level - allow to vary depending on data
  if(length(ww10) > 1){ # if many then take 1/3 of range above and below
    max_who <- max(who[ww10,"mdr_new"]); min_who <- min(who[ww10,"mdr_new"])
    dist_who <- max_who - min_who
    final_levels <- seq(min_who - 2*dist_who, max_who + 2*dist_who, length.out = 100) # do from min - 2*distance between to max + 2*dist between
    final_levels <- final_levels[final_levels > 0] # remove negative ones}
  }
  if(length(ww10) == 1){ # if one then take 1/3 above and below this single point
    fl1 <- who[ww10,"mdr_new"]
    final_levels <- seq(fl1-2*(fl1), fl1 + 2*fl1, length.out = 100) # do from min minus 2*distance between to max + 2*dist between
  }
  if(length(ww10) < 1){final_levels <- head(who,1)$mdr_new} # Just last point #mdr_last0[which(mdr_last0$iso3==cc),"mdr_new_pcnt"]/100} # original mdr_last
  
  ## Cycle through final levels of MDR in 2014
  if(length(final_levels) > 0){ 
    
    nind <- length(unique(save_curves$index))
    ll <- matrix(0,nind * length(final_levels),4)
    indexll <- 1
    
    for(j in 1:length(final_levels)){
      mdr_last <- final_levels[j]
      save_curves$out <- mdr_last * save_curves_lin$out/0.02 # rescale for this country 
      
      ## Likelihood
      for(k in 1:nind){
        w <- which(save_curves$index == k)
        pot_curve <- save_curves[w,]
        ll[indexll,1] <- k
        ll[indexll,2] <- likelihood(who, pot_curve)
        ll[indexll,3] <- mdr_last 
        ll[indexll,4] <- j 
        indexll <- indexll + 1
      }
    }
    #plot(ll[,3],ll[,2])
  }
  
  ll_all <- rbind(ll_all,cbind(i, ll))
  
}

ll_all <- as.data.frame(ll_all)
colnames(ll_all) <- c("cn","nind","likelihood_v","mdr_last","mdr_last_index")
write.csv(ll_all,"~/Dropbox/MDR/output/likelihood_data.csv")

# Find maximum likelihood for each country over all 
# ind = all potential curve shapes
# mdr_last = all potential end points for shapes
ll_max <- as.data.frame(ll_all %>% group_by(cn) %>% slice(which.max(likelihood_v)))

hist(ll_max$nind) # many have the linear increase from 1970

##**** For one country check ****************************************************************######
cn_id = 1
# Likelihoods
ll_all_cn <- subset(ll_all, ll_all$cn == cn_id)
ggplot(ll_all_cn, aes(x=mdr_last, y = likelihood_v)) + geom_point()
# WHO data
who <- as.data.frame(who0[which(who0$iso3 == cn[cn_id]),c("year","mdr_new","mhi","mlo")])
# Likelihood data
ll_low <- subset(ll_max, ll_max$cn == cn_id)
mdr_l <- ll_low[,"mdr_last"] # mdr_level at min
# Best curves
save_curves<-save_curves_lin
save_curves$best <- 0
save_curves$out <- mdr_l * save_curves_lin$out/0.02
save_curves[which(save_curves$index == ll_low$nind),"best"] <- 1
who$best <- 0
ggplot(save_curves, aes(x=year, y = out, group = index, col = factor(best))) + geom_line() +
  geom_point(data = who, aes(x = year, y = mdr_new, group = ""), col = "blue") + geom_errorbar(data = who, aes(group = "",y = mdr_new, ymin=mlo, ymax = mhi), col="blue") + 
  scale_color_manual("Curves with\nbest final\nmdr level",values = c("grey","red"), labels = c("all","best fit")) + 
  scale_x_continuous("Year") + scale_y_continuous("Proportion new with MDR")

# All curves
mdrs <- unique(subset(ll_all, ll_all$cn == cn_id)["mdr_last"])
mm <- unique(save_curves$index)
ll <- length(which(save_curves$index == 1))
ss <- c()
for(i in 1:length(mdrs[,1])){
  save_curves$out <- mdrs[i,] * save_curves_lin$out/0.02
  ss <- rbind(ss, cbind(save_curves,rep(((i-1)*mm):(i*mm-1),each = ll)))
}
ss <- as.data.frame(ss)
colnames(ss) <- c(colnames(save_curves),"nrun")
ss$best = 2
who$best = 3
ggplot(save_curves, aes(x=year, y = out, group = index, col = factor(best))) + geom_line() +
  geom_point(data = who, aes(x = year, y = mdr_new, group = "")) + geom_errorbar(data = who, aes(group = "",y = mdr_new, ymin=mlo, ymax = mhi)) + 
  geom_line(data = ss, aes(x = year, y = out, group = nrun),alpha = 0.02) + 
  scale_color_manual("",values = c("black","red","grey","blue"), 
                     labels = c("Curves best final mdr","Lowest Likelihood","All curves","WHO data")) +
  scale_x_continuous("Year") + scale_y_continuous("Proportion new with MDR")



##**** For one country check ****************************************************************######
ll_all <- as.data.table(ll_all)
ll_all %>% group_by(cn) %>% sample(ll_all,10, prob=likelihood_v, replace = TRUE)

ll_all_cn$likeli_corr <- ll_all_cn$likelihood_v/sum(ll_all_cn$likelihood_v)
s <- sample(seq(1,length(ll_all_cn$cn),1), 1000, prob = ll_all_cn$likeli_corr, replace = TRUE)
hist(ll_all_cn[s,"mdr_last"], breaks = seq(0,max(ll_all_cn$mdr_last)+0.02,0.01))
ggplot(ll_all_cn, aes(x=mdr_last, y = likeli_corr)) + geom_point()
ggplot(ll_all_cn, aes(x = seq(1,2870,1), y = likelihood_v)) + geom_point()
hist(s)

somax <- exp(ll_all_cn$likelihood_v)/sum(exp(ll_all_cn$likelihood_v)) # deals with negative likelihoods but weights oddly
plot(ll_all_cn$likelihood_v, somax)
s <- sample(seq(1,length(ll_all_cn$cn),1), 1000, prob = somax, replace = TRUE)
hist(s, breaks = seq(1,length(ll_all_cn$cn),1))

hist(sample(c(1,2,3),1000,prob = c(1,2,3), replace = TRUE))
# sort ascending: s <- sort(sum_ls$x, index.return=TRUE); best_all = rbind(best_all, cbind(final_levels[j],sum_ls[s$ix[1:10],])) # returns top 10
ls_best <- as.data.frame(sum_ls %>% group_by(Type) %>% slice(which.min(x)))

best_all = rbind(best_all, cbind(final_levels[j],ls_best)) # returns top 1 from each type

}
best_all <- as.data.frame(best_all)
colnames(best_all) <- c("f_l", colnames(sum_ls))
colnames(ss)[1] <- "f_l"

## Finding the best curves
# which one has minimum
best <- best_all[which.min(best_all$x),]

# find minimum for each type across all f_l
s <- as.data.frame(best_all %>% group_by(Type) %>% slice(which.min(x)))

# Where is the data for these best curves?
w1<-intersect(which(ss$index == s[1,"Index"]),intersect(which(ss$type == s[1,"Type"]),which(ss$f_l == s[1,"f_l"])))
w2<-intersect(intersect(which(ss$index == s[2,"Index"]), which(ss$type == s[2,"Type"])),which(ss$f_l == s[2,"f_l"]))
w3<-intersect(intersect(which(ss$index == s[3,"Index"]), which(ss$type == s[3,"Type"])),which(ss$f_l == s[3,"f_l"]))
w4<-intersect(intersect(which(ss$index == s[4,"Index"]), which(ss$type == s[4,"Type"])),which(ss$f_l == s[4,"f_l"]))

# Keep the least squares distance in the data for these best curves
ss[w1,"dist"]<- s[1,"x"];ss[w2,"dist"]<- s[2,"x"];
ss[w3,"dist"]<- s[3,"x"];ss[w4,"dist"]<- s[4,"x"];

## Plot
# What does the best one look like?
w<-intersect(which(save_curves$index == best$Index), which(save_curves$type == best$Type))
save_curves$out <- best$f_l * save_curves0$out/ 0.02
# ggplot(save_curves[w,], aes(x=year, y = out)) + geom_line() + 
#   geom_point(data = who,aes(x=year, y = mdr_new))
# ggsave(paste0(cc,"_p_best_fit.pdf"), width = 32, height = 24)

# Plot best in different colour against all with same f_l
save_curves$best <- 0
save_curves[w,"best"] <- 1
who$index = 1
# p1 <- ggplot(save_curves, aes(x=year, y = out, group = index)) + geom_line(aes(colour = factor(best),alpha = factor(best))) + facet_wrap(~type) +
#  geom_point(data = who,aes(x=year, y = mdr_new_pcnt/100),col="blue") + scale_colour_manual(values = c("black","red"))
# ggsave(paste0(cc,"_p1.pdf"), width = 32, height = 24)

# Plot all f_l levels and best for each type
ss$best <- 1; ss$trans <- 0
ss[c(w1,w2,w3,w4),"best"]<- 0; ss[c(w1,w2,w3,w4),"trans"]<- 1
ss$dist <- 0
who$f_l <- 1
# p2 <- ggplot(ss, aes(x=year, y = out, group = interaction(f_l,index))) + geom_line(aes(colour = factor(best), alpha = factor(trans))) + facet_wrap(~type, scales="free") +
#   geom_point(data = who,aes(x=year, y = mdr_new_pcnt/100),col="blue") + scale_colour_manual(values = c("red","black")) +
#   scale_x_continuous("MDR ARI level")
# # ggsave(paste0(cc,"_p2.pdf"), width = 32, height = 24)
# 
# p3 <- ggplot(ss[c(w1,w2,w3,w4),], aes(x=year, y = out, group = interaction(f_l,index))) + geom_line(aes(colour = factor(best))) + facet_wrap(~type, scales="free") +
#   geom_point(data = who,aes(x=year, y = mdr_new_pcnt/100),col="blue") + scale_colour_manual(values = c("red","black")) + 
#   scale_x_continuous("MDR ARI level") 
# ggsave(paste0(cc,"_p3.pdf"), width = 32, height = 24)
} else{ best[1:4] <- 0}

# Store curves for best ones
store_cn_best <- rbind(store_cn_best, cbind(i,s,dim(who)[1]))

}

store_cn_best0 <- as.data.frame(store_cn_best)
store_cn_best <- as.data.frame(store_cn_best)
colnames(store_cn_best) <- c("country", "f_l","Index","Type","dist","n_data")
store_cn_best$country <- cn[store_cn_best$country]

w0 <- which(store_cn_best$Type == 0)
w1 <- which(store_cn_best$Type == 1)
w2 <- which(store_cn_best$Type == 2)
w3 <- which(store_cn_best$Type == 3)
store_cn_best$mdr_rep <- 0
store_cn_best[w0,"mdr_rep"]<- as.numeric(store_cn_best[w0,"Index"])
store_cn_best[w1,"mdr_rep"]<- as.numeric(store_cn_best[w1,"Index"]) + length(w0)
store_cn_best[w2,"mdr_rep"]<- as.numeric(store_cn_best[w2,"Index"]) + length(w1)
store_cn_best[w3,"mdr_rep"]<- as.numeric(store_cn_best[w3,"Index"]) + length(w2)
#store_cn_best <- apply(store_cn_best,2,as.character)
write.csv(store_cn_best, "store_cn_best.csv")




