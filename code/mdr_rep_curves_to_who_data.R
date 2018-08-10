##### Compare to data
library(ggplot2); library(dplyr)
theme_set(theme_bw(base_size=24))

# compare trends used to data
#save_curves0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent MDR/Data/lin_sig_quad_curves.csv",stringsAsFactors = FALSE)[-1]
save_curves0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent_MDR/Data/lin_sig_quad_sigd_curves.csv",stringsAsFactors = FALSE)[-1]
mdr_last0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/data/mdr_last.csv",stringsAsFactors = FALSE)[-1]
who0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/new_who_edited.csv",stringsAsFactors = FALSE)[-1]

setwd("~/Documents/LTBI_MDR/output/")

# Countries to look at 
cn <- unique(mdr_last0$iso3)

who0$year <- who0$year_new

store_cn_best <- c()

for(i in 1:length(cn)){
  
  ## iso3 which country?
  cc <- cn[i]
  print(cc)
  
  ## data - for this country
  who <- who0[which(who0$iso3 == cc),c("year","mdr_new_pcnt")]
  who$mdr_new<- who$mdr_new_pcnt/100
  
  ## What data points past 2000? 
  who$year0 <- who$year
  ww10 <- which(who$year > 2000)
  
  ## Curves for this final_level
  save_curves <- save_curves0
  
  ## final level - allow to vary depending on data
  if(length(ww10) > 1){ # if many then take 1/3 of range above and below
    max_who <- max(who[ww10,"mdr_new"]); min_who <- min(who[ww10,"mdr_new"])
    dist_who <- max_who - min_who
    final_levels <- seq(min_who - dist_who/3, max_who + dist_who/3, length.out = 10) # do from min minus third distance between to max + third dist between
    final_levels <- final_levels[final_levels > 0] # remove negative ones}
  }
  if(length(ww10) == 1){ # if one then take 1/3 above and below this single point
    fl1 <- who[ww10,"mdr_new"]
    final_levels <- seq(fl1-(fl1)/3, fl1 + fl1/3, length.out = 10) # do from min minus third distance between to max + third dist between
  }
  if(length(ww10) < 1){final_levels <- mdr_last0[which(mdr_last0$iso3==cc),"mdr_new_pcnt"]/100} # original mdr_last
  
  ## Cycle through final levels of MDR in 2014
  if(length(final_levels) > 0){ 
    best_all <- c(); ss <- c();
    
    for(j in 1:length(final_levels)){
      mdr_last <- final_levels[j]
      save_curves$out <- mdr_last * save_curves0$out/0.02 # rescale for this country 
      ss <- rbind(ss, cbind(final_levels[j],save_curves))
    
      ## Distance
      # dim(dd) = size(who) * 129 (number of reps in save_curves)
      ww <- which(who$year > 2014)
      if(length(ww) > 0){who[ww,"year"] <- 2014} 
      dd <- merge(who, save_curves, by = "year")
      dd$dist <- (dd$mdr_new - dd$out)^2
      # type = 0 = linear, 1 = sigma, 2 = quad
      sum_ls <- aggregate(dd$dist, by=list(Index=dd$index, Type = dd$type), FUN=sum)
      # ggplot(sum_ls, aes(x=Index, y = x, group = factor(Type))) + geom_point(aes(colour=factor(Type)))
      
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




