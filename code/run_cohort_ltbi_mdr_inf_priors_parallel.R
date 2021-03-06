#### Run cohort_ltbi_mdr parallel on my mac



run_cohort_parallel <- function(cci){
  ###********** Libraries and ggplot theme ************************************************************************************************#######
  library(ggplot2)
  theme_set(theme_bw(base_size = 24))
  library(plyr)
  library(dplyr)
  library(cowplot)
  library(data.table)
  library(reshape2)
  library(R.devices)
  ###********** Home ************************************************************************************************#######
  home <- "~/Documents/LTBI_MDR/"
  setwd(home)
  #output <- "~/Documents/LTBI_MDR/output"
  output <- "~/Dropbox/MDR/output" # TEMPORARY - can be shared between computers then
  
  ###********** Load code and data ************************************************************************************************#######
  source("code/cohort_ltbi_mdr.R") # loads function for underyling cohort model
  
  ## Population size 2014
  load('data/POP2014.Rdata')  
  
  ###********** Run for different countries ************************************************************************************************************************#######
  
  ## WHO data
  w_data <- read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1]
  
  ## Which countries? 
  cni <- read.csv("~/Dropbox/MDR/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
  length(cni) # 138 now
  llu <- length(cni)
  
  # ##### DO JUST FOR FIRST FIVE! *******************************************************************************************
  # llu <- 5 
  # #w <- which(all0$mdr_ari > 0) 
  # #cni <- unique(all0[w,"iso3"])
  # ##### DO JUST FOR FIRST FIVE! *******************************************************************************************
  
  cni_rem <- c() # blank to store what else to remove
  
  nari = 200 # up to 200
  
  # DS and MDR data
  # Label for plots 
  pp <- "infor_prior"
  
  # Store all? 
  store_all <- as.data.frame(matrix(0,length(cni)*4*81*100,10))
  runn <- 1
  level2014 <- c(); #breakdown proportions infected by age
  s_level <- c(); #sum proportions infected 
  
  # READ IN
  load("~/Dropbox/MDR/output/all0_p_ds_mdr.Rdata")
  
  sa <- c() # store for this country
  #print(c(cci,cni[cci]))
  
  ### WHO data
  d <-subset(w_data, iso3 == as.character(cni[cci]) )
  
  
  #if(!file.exists(paste0("~/Dropbox/MDR/output/",cni[cci],"s_level_",nari,"_",pp,".csv"))){
  ### ARI for both DS and mDR in all0
  rdata <- all0[which(all0$iso3 == as.character(cni[cci])),]
  
  a1 <- ggplot(d, aes(x=year_new, y = new_mdr_prop),col="red",pch = 10) + # points won't plot over lines unless do points first?!
    geom_point() +
    geom_line(data = rdata, aes(x=year, y = prediction, group = factor(replicate)),alpha = 0.2) +
    scale_y_continuous("Prop. new with MDR") + scale_x_continuous("Year",lim=c(1970,2015)) +
    geom_point(data = d, aes(x=year_new, y = new_mdr_prop),col="red",pch = 10) + geom_errorbar(data = d, aes(ymin = mlo, ymax = mhi), col = "red")
  R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/",cni[cci],"_mdr_trends_with_data_",pp,".pdf"),width=11, height=11))
  
  a2 <- ggplot(rdata, aes(x=year, y = mdr_ari, group = factor(replicate))) + geom_line(alpha = 0.2) +
    scale_y_continuous("MDR ARI") + scale_x_continuous("Year",lim=c(1970,2015))
  R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/",cni[cci],"_mdr_ari_",pp,".pdf"),width=11, height=11))
  
  for(i in 1:nari){
    #print(c(i,"ari rep"))
    ari <- rdata[which(rdata$replicate == i),c("ds_ari","mdr_ari")]
    colnames(ari) <- c("ds","mdr")
    pop <- as.data.frame(POP2014[which(as.character(POP2014$iso3) == as.character(cni[cci])),"value"])
    
    cc <- cohort_ltbi(ari, pop)
    
    combs <- cc$combs
    
    # by age
    combs$mdr_rep <- i
    combs$age_group <- seq(1:17)
    combs$popf <- cci
    #level2014 <- rbind(level2014,combs)
    level2014 <- rbind(level2014,cbind(cc$c_2014,combs[1,c("mdr_rep","popf")],row.names = NULL))
    
    
    # total percentage infected sums
    ltbi_dr <- sum(combs$perc_dr) # percentage infected
    ltbi_ds <- sum(combs$perc_ds)
    pltbi_dr <- sum(combs$size_dr) # number of people infected
    pltbi_ds <- sum(combs$size_ds)
    
    ltbi_dr_kids <- sum(combs$perc_dr[1:3]) # percentage infected
    ltbi_ds_kids <- sum(combs$perc_ds[1:3])
    pltbi_dr_kids <- sum(combs$size_dr[1:3]) # number of people infected
    pltbi_ds_kids <- sum(combs$size_ds[1:3])
    
    # Bind together.
    s_level <- rbind(s_level,c(i,ltbi_dr,ltbi_ds,cci,pltbi_dr, pltbi_ds,ltbi_dr_kids,ltbi_ds_kids, pltbi_dr_kids, pltbi_ds_kids, sum(pop), sum(pop[1:3,1])))
    
    ssc <- cc$store_c
    lowi <- ((runn-1)*(dim(ssc)[1])+1)
    uppi <- ((runn)*(dim(ssc)[1]))
    store_all[lowi:uppi,1] <- i;
    store_all[lowi:uppi,2] <- cni[cci];
    store_all[lowi:uppi,3:10] <- ssc
    
    runn <- runn + 1
    sa <- rbind(sa,cbind(i,cni[cci], ssc)) # just for this country
  }
  
  # store all for this country
  sa <- as.data.frame(sa)
  colnames(sa) <- c(c("mdr_rep","cn"),colnames(cc$store_c))
  write.csv(sa, paste0("~/Dropbox/MDR/output/",cni[cci],"_sa_",nari,"_",pp,".csv"))
  
  # Just recent infection
  w<-which(sa$year > 2012)
  write.csv(sa[w,], paste0("~/Dropbox/MDR/output/",cni[cci],"_rec_infec_",nari,"_",pp,".csv"))
  
  
  s_level <- as.data.frame(s_level)
  colnames(s_level) <- c("rep","ltbir","ltbis","popf","pltbir", "pltbis","ltbir_kids","ltbis_kids",
                         "pltbir_kids", "pltbis_kids","pop","pop_kids")
  
  dim(level2014) #nari * 107
  level2014$cn <- cni[as.numeric(level2014$popf)]
  write.csv(level2014, paste0("~/Dropbox/MDR/output/",cni[cci],"level2014_",nari,"_",pp,".csv"))
  
  s_level0 <- s_level
  s_level$pop_name <- cni[as.numeric(s_level0$popf)]
  s_level <- as.data.table(s_level)
  write.csv(s_level, paste0("~/Dropbox/MDR/output/",cni[cci],"s_level_",nari,"_",pp,".csv"))
  
  
  #}
  
  ################**######################################################################################################################################################################################################
  ################**######################################################################################################################################################################################################
  ################**######################################################################################################################################################################################################
  ###******* AGE *****################################################################################################################
  ################**######################################################################################################################################################################################################
  ################**######################################################################################################################################################################################################
  ################**######################################################################################################################################################################################################
  
  if(!file.exists(paste0("~/Dropbox/MDR/output/eg_all_age_",cni[cci],"_r_",labl,".pdf"))){  
    ss_mean <- c()
    
    #print(c(cci,cni[cci]))
    
    # Read in all data for this country
    sa <- read.csv(paste0("~/Dropbox/MDR/output/",cni[cci],"_sa_",nari,"_infor_prior.csv"))[,-1]
    
    # 2014 level
    level2014 <- read.csv(paste0("~/Dropbox/MDR/output/",cni[cci],"level2014_",nari,"_",pp,".csv"))[,-1]
    
    sa_rec <- sa[which(sa$year > 1965),] # recent
    
    labl = "infor_prior"
    
    a<-ggplot(sa_rec[which(sa_rec$mdr_rep < 5),], aes(x=year, y = pr_dr, group = age, col = age)) +
      geom_line() + facet_wrap(~mdr_rep, ncol = 5) +
      scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkorange1","darkgreen"))
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/eg_all_age_",cni[cci],"_r_",labl,".pdf"), height = 10, width = 10))
    
    a<-ggplot(sa_rec[which(sa_rec$mdr_rep < 5),], aes(x=year, y = pr_ds, group = age, col = age)) +
      geom_line() + facet_wrap(~mdr_rep, ncol = 5) +
      scale_colour_gradientn(limits = c(0,100),colours=c("navyblue", "darkmagenta", "darkgreen"))
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/eg_all_age_",cni[cci],"_s_",labl,".pdf"), height = 10, width = 10))
    
    a<-ggplot(sa_rec[which(sa_rec$mdr_rep < 5),], aes(year, age)) +
      geom_tile(aes(fill = pr_ds),colour = "white") +
      scale_fill_gradient(low = "white",high = "steelblue","DS-TB") +
      labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/eg_all_age_",cni[cci],"_map_s_",labl,".pdf"), height = 10, width = 10))
    
    a<-ggplot(sa_rec[which(sa_rec$mdr_rep < 5),], aes(year, age)) +
      geom_tile(aes(fill = pr_dr),colour = "white") +
      scale_fill_gradient(low = "white",high = "red4","MDR-TB") +
      labs(x = "Year",y = "Age") + scale_y_continuous(lim = c(0,100)) + facet_wrap(~mdr_rep)
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/eg_all_age_",cni[cci],"_map_r_",labl,".pdf"), height = 10, width = 10))
    
    
    ####*********** When contributes most to latent burden? *********######################################################################################################################################################
    age_groups <- cbind(seq(1,85,5),seq(5,85,5))
    age_groups[17,2] <- 100 # last one 81 - 100 years old
    
    # This countries data for each age and year
    s <- sa
    # level at 2014 for this country - read in!
    #l14 <- level2014[which(as.character(level2014$cn) == as.character(cni[cci])),] # don't need
    l14 <- level2014
    
    # where store?
    s_new <- c()
    
    for(k in 1:nari){# for each mdr_rep
      # print(c("mdr_rep",k))
      # subset the yearly data
      s_k <- subset(s, mdr_rep == k)
      
      for(j in 1:100){# all ages
        #print(c("age",j))
        
        # subset the proportion infected by mdr_rep and age group
        l14_k_j <- subset(l14, mdr_rep == k)[j,] # proportion with dr in 2014 for mdr_rep = k and age = j
        
        s_temp <- c()
        # for each year get the data for those that are that age in 2014
        for(yr in 2014:1934){
          
          agen = j - (2014-yr) # age in that year
          
          if(agen > 0){ # need age > 0!
            s_temp <- rbind(s_temp,subset(s_k, year == yr)[agen,]) # remeber this is age in 2014
          }
        }
        
        ## Cumulative change in proportion with DR or DS
        ## OK for this to go negative... as suggests proportion is decreasing.
        s_temp$cumr_py <- s_temp$new_dr - s_temp$rei_rs + s_temp$rei_sr
        s_temp$cums_py <- s_temp$new_ds - s_temp$rei_sr + s_temp$rei_rs
        
        w<-which(s_temp$year == 1934)
        if(length(w) > 0 ){s_temp[w,"cums_py"] <- 0} # remove initial infections prior to 1934}
        
        
        ### Gives the right proportion as in l14_k_j
        # s_temp <- as.data.frame(s_temp)
        # colwise(sum)(s_temp)[,"new_dr"] - colwise(sum)(s_temp)[,"rei_rs"] + colwise(sum)(s_temp)[,"rei_sr"]
        # tail(s_temp,1)[,"pr_ds"] + colwise(sum)(s_temp)[,"new_ds"] - colwise(sum)(s_temp)[,"rei_sr"] + colwise(sum)(s_temp)[,"rei_rs"]
        # tail(s_temp,1)[,"pr_ds"] + sum(s_temp$new_ds) - sum(s_temp$rei_sr) + sum(s_temp$rei_rs)
        # sum(s_temp$cumr_py)
        # sum(s_temp$cums_py)
        # l14_k_j
        
        ## proportion of amount in 2014 that is from this cumulative change
        s_propr <- matrix(0,81,1);   s_props <- matrix(0,81,1)
        if(l14_k_j$pr_dr > 0){
          w<-which(s_temp$cumr_py > 0) # only take those that add to the LTBI burden
          s_propr[w,] <- s_temp[w,"cumr_py"] / sum(s_temp[w,"cumr_py"])} # Divide by added LTBI not final total. #s_temp$cumr_py /l14_k_j$pr_dr}
        
        w<-which(s_temp$cums_py > 0) # only take those that add to the LTBI burden
        s_props[w,] <- s_temp[w,"cums_py"] / sum(s_temp[w,"cums_py"]) # Divide by added LTBI not final total. #s_temp$cumr_py /l14_k_j$pr_dr}
        
        #Don't need to do this now as just cumulative increase s_props <- s_temp$cums_py / (l14_k_j$pr_ds - tail(s_temp,1)[,"pr_ds"]) # remove level in 1934 if there was any
        
        s_npropr <- matrix(0,81,1);
        s_nprops <- matrix(0,81,1);
        for(kk in 1:length(s_props)){
          s_npropr[kk] <-  s_propr[kk]
          s_nprops[kk] <-  s_props[kk]
        }
        
        #s_new <- rbind(s_new,(cbind(s_npropr, s_nprops, seq(2014,1934,-1),j,k)))
        ## should be 1
        #sum(s_props)
        #sum(s_propr)
        
        
        s_new <- rbind(s_new,(cbind(
          s_npropr, s_nprops, # proportion of amount in 2014 that is from this cumulative change
          seq(2014,1934,-1),j,k # years, age, rep
        )))
      }
      
    }
    
    ## All data
    s_new <- as.data.frame(s_new)
    colnames(s_new)<-c("pr_r","pr_s","year","age","mdr_rep")
    
    ## s_new: each row has the age in 2014 the year from which some contribution may come
    # and the size of the contribution
    write.csv(s_new,paste0("~/Dropbox/MDR/output/s_all_",cni[cci],"_",labl,".csv"))
    #s_all <- read.csv("s_all.csv", stringsAsFactors = FALSE)[,-1]
    
    ### Exploring plots
    ## cumr can go negative - re-infections v important. Proportion infected with R can decrease!
    # plot(s_temp[,"year"],s_temp[,"cumr_py"]) # total new each yar
    # lines(s_temp[,"year"],s_temp[,"new_dr"]) # new infections
    # lines(s_temp[,"year"],s_temp[,"rei_sr"],col="blue") # additional reinfections
    # lines(s_temp[,"year"],s_temp[,"rei_rs"],col="red") # removed reinfections with S
    # abline(h = 0, lty = "dashed")
    
    ### s_new => has the proportion from each year at each age for each mdr_rep
    w<-intersect(which(s_new$age == 32),which(s_new$mdr_rep == 1))
    sum(s_new[w,"pr_r"]) # = 1
    sum(s_new[w,"pr_s"]) # = 1
    
    
    ## plot: cumulative bar chart
    ## x axis = age [0-100]
    ## y axis = proportion ltbir from each time point [0-1]
    ## facet by mdr rep
    
    ## Want: of all LTBI, when was it gained? So need not just by age... but by total population.
    s_new$pr_ltbir <- 0
    s_new$pr_ltbis <- 0
    s_new$yearcat<-cut(s_new$year, seq(1929,2018,5))
    
    
    ## For each country get population distribution
    pop <-  POP2014[which(as.character(POP2014$iso3) == as.character(cni[cci])),"value"]
    
    ## For the population in 2014. Calculate the proportion of the total population in each yearly age group.
    pr_2014_age = pop / sum(pop) / 5 ## Divided by 5 to make per subunit (think works as equivalent to averaging proportions and multiplying by total)
    pr_2014_age[17] = pop[17] / sum(pop) / 20 ## Apart from last which is 20 yrs long
    
    ss_here <- s_new
    
    for(i in 1:100){
      w <- which(ss_here$age == i)
      m <- intersect(which(age_groups[,1] <= i), which(age_groups[,2] >= i))
      ss_here[w,"pr_ltbir"] = ss_here[w,"pr_r"] * as.numeric(pr_2014_age[m])
      ss_here[w,"pr_ltbis"] = ss_here[w,"pr_s"] * as.numeric(pr_2014_age[m])
    }
    
    ## Grouped by mdr_rep
    # w<-which(ss_here$mdr_rep == 5)
    # sum(ss_here[w,"pr_ltbis"]) # = 1
    # sum(ss_here[w,"pr_ltbir"]) # = 1
    
    setwd(output)
    ## This says: by age, when were they infected. The proportion of their % infected that can be
    # allocated to past times.
    a<-ggplot(ss_here[which(ss_here$mdr_rep < 5),], aes(age, pr_s, fill = factor(year))) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous() + facet_wrap(~mdr_rep) + ggtitle(paste0("DS-TB, ",cni[cci])) +
      scale_fill_hue("clarity")
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/eg_DS_age_",cni[cci],"_ltbis_when_",labl,".pdf"), height = 10, width = 10))
    
    a<-ggplot(ss_here[which(ss_here$mdr_rep < 5),], aes(age, pr_r, fill = factor(year))) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous() + facet_wrap(~mdr_rep) + ggtitle(paste0("MDR-TB, ",cni[cci])) +
      scale_fill_hue("clarity")
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/eg_DR_age_",cni[cci],"_ltbir_when_",labl,".pdf"), height = 10, width = 10))
    
    
    ## This says: by mdr_rep, when is the time window that contributes most
    ## Tried to highlight 1980 period... but not working
    #w <- which(ss_here$yearcat == "(1989,1994]")
    #ss_here$extra_label_fill <- 0
    #ss_here[w,"extra_label_fill"] <- 1
    #scale_colour_manual( values = c( "1"="black","0" = "white"), guide = FALSE )
    
    a<-ggplot(ss_here, aes(mdr_rep, pr_ltbis, fill = factor(yearcat))) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous("Percentage of LTBI DS\nfrom this 5 year time interval") +
      ggtitle("DS-TB") +
      scale_fill_hue("Year")
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/DS_",cni[cci],"_ltbis_when_",labl,".pdf"), height = 10, width = 20))
    
    a<-ggplot(ss_here,aes(mdr_rep, pr_ltbir, fill = factor(yearcat))) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous("Percentage of LTBI MDR\nfrom this 5 year time interval") +
      ggtitle("MDR-TB") +
      scale_fill_hue("Year")
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/DR_",cni[cci],"_ltbir_when_",labl,".pdf"), height = 10, width = 20))
    
    # Average over yearcat
    meanv <- ss_here %>% group_by(yearcat,mdr_rep) %>%
      dplyr::summarise(sum_prltbir = sum(pr_ltbir), sum_prltbis = sum(pr_ltbis))
    
    write.csv(meanv, paste0("~/Dropbox/MDR/output/",cni[cci],"_props_ltbi_when_",labl,".csv"))
    
    # Average over all reps
    means <- ss_here %>% group_by(yearcat,mdr_rep) %>%
      dplyr::summarise(totals = sum(pr_ltbis)) %>%
      ungroup %>%
      group_by(yearcat) %>%
      dplyr::summarise(mean=mean(totals), min=quantile(totals, 0.025), max=quantile(totals, 0.975))
    
    meanr <- ss_here %>% group_by(yearcat,mdr_rep) %>%
      dplyr::summarise(totals = sum(pr_ltbir)) %>%
      ungroup %>%
      group_by(yearcat) %>%
      dplyr::summarise(mean=mean(totals), min=quantile(totals, 0.025), max=quantile(totals, 0.975))
    
    means$type <- 0
    meanr$type <- 1
    mean_both <- rbind(means,meanr)
    
    a<-ggplot(mean_both, aes(x= yearcat, y= mean, fill = factor(type) )) +
      geom_bar(stat = "identity", position = "dodge") + geom_errorbar(aes(ymin = min, ymax = max), position = "dodge") +
      scale_fill_discrete("TB type",labels = c("DS","MDR")) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete("Year grouping") + scale_y_continuous("Contribution of this year\nto LTBI burden")
    R.devices::suppressGraphics(ggsave(paste0("~/Dropbox/MDR/output/DR_mean_",cni[cci],"_ltbir_when_",labl,".pdf"), height = 10, width = 20))
    
    ss_mean <- rbind(ss_mean, cbind(mean_both,cni[cci]))
    
    write.csv(ss_mean, paste0("~/Dropbox/MDR/output/",cni[cci],"_ss_mean_",nari,"_",labl,".csv"))
    
  }
}  