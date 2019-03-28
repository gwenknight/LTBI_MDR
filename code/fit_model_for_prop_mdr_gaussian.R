### Code to fit y = bt - ct^2 to WHO data for all countries

###********** Libraries and ggplot theme ************************************************************************************************#######
library('rstan')
library('dplyr')
library('ggmcmc')
library("boot")
library("reshape")
library("ggforce")
library("brms")
theme_set(theme_bw(base_size = 24))

###********** Home ************************************************************************************************#######
home <- "~/Documents/LTBI_MDR/"
setwd(home)

###********** Load code and data ************************************************************************************************#######
# Country list and WHO data
## NOT SUPPLIED WITH REPOSITORY
who0 <- as.data.frame(read.csv("data/new_who_edited_sub.csv")[,-1]) 
#who0$year <- who0$year_new

# Add in sigma (standard deviation) to data
who0$mdr_new <- who0$new_mdr_prop
who0$sigma <- (who0$mhi - who0$mdr_new)/1.96  

## JUST doing for 3 countries now: India / China / USA
#uu <- read.csv("data_final/138_final_list_included_countries.csv",stringsAsFactors = FALSE)[,-1]
uu <- read.csv("data/3_spline_included_countries.csv",stringsAsFactors = FALSE)[,-1]
luu <- 3 #length(unique(who0$iso3)) # 138

# Store
pp_store <- c() # only three countries, not so big

#pp <- "original"

# Cycle through all countries
for(ii in 1:luu) {
  # country for this fit 
  cnn <- uu[ii]
  
  # data for this country with additional zeros 1965 - 1970
  who_cnn <- subset(who0, iso3 == cnn)[,c("year_new","mdr_new")]
  zero_pre <- as.data.frame(cbind(seq(1965,1970,1),0))
  colnames(zero_pre) <- colnames(who_cnn)
  who_cnn <- rbind(zero_pre,who_cnn)
  
  who_cnn <- who_cnn[,c("year_new","mdr_new")]
  
  years_to_predict <- seq(1970, 2018) # NEED?
  print(c(ii,as.character(country),nrow(who_cnn)))
  
  ## Fit a gaussian process to the data
  fit_smooth1 <- brm(
    bf(mdr_new ~ s(year_new)),
    data = who_cnn, family = gaussian(),
    chains = 2,iter = 4000, warmup = 1000, thin = 10, control = list(adapt_delta = 0.95)
  )
  
  # ## With informative Priors
  # posterior.p<-As.mcmc.list(m.p,pars=c("b", "t_m","rho"))
  # ggmcmc(ggs(posterior.p), file=paste0("output_spline/",country, "_inforprior_mcmc.pdf"))
  # 
  
  # Plot samples
  pdf(paste0("output_spline/",cnn,"_marginaleffects.pdf"))
  plot(marginal_effects(fit_smooth1), points = TRUE, ask = FALSE)
  dev.off()
  
  # 200 samples for predicted levels 
  #samples.p.p <- rstan::extract(m.p, pars="p_pred", permuted = FALSE)[801:1000,1,] # pick first chain
  data <- as.data.frame(cbind(seq(1965,2015,1),0))
  colnames(data) <- c("year_new","blank")
  pp <- t(posterior_predict(fit_smooth1, data))
  ppc <- as.data.frame(cbind(data$year_new, pp))
  colnames(ppc) <- c("year",seq(1,600,1))
  
  ppc2 <- ppc
  ppc2 <- replace(ppc2, ppc2 < 0, 0)
  
  ppcm <- melt(ppc2[,1:201], id = "year")
  
  ggplot(ppcm, aes(x=year, y = value, group = variable)) + geom_line() + geom_point(data = who_cnn, aes(x=year_new, y = mdr_new, group = 0), col = "red")
  ggsave(paste0("output_spline/",cnn,"all_fits.pdf"))
  
  # save 
  ppcm$iso3 <- cnn
  
  pp_store <- rbind(pp_store, ppcm)
}

write.csv(pp_store,"output_spline/all_samples_p.csv")

##### make correct for adding to other data
# and merge
load("../MDR-LTBI-paper/data_final/rundata_ari_1000.Rdata") # From running the code in Houben & Dodd in 2016.
rundatar$ari <- exp(rundatar$lari)

# # Rename
# all_samples <- all_samples_p %>%
#   mutate(replicate = sample, iso3 = country) %>%
#   dplyr::select(-sample) %>% dplyr::select(-country)
# # Set negative to 0
# all_samples[which(all_samples$prediction < 0), "prediction"] <- 0

# # Set greater than 1 to 1! Data on proportion (0-1) so cannot go above 1. 
# all_samples[which(all_samples$prediction > 1), "prediction"] <- 1

# Add in 0 before 1970
ns <- max(as.numeric(unique(pp_store$variable)))
sample_v <- seq(1,ns,1)
years <- unique(rundatar$year) 
years_v <- years[which(years < 1965)]
ny <- length(years_v)
nc <- length(unique(pp_store$country))
pre_1970_mdr <- as.data.frame(cbind(rep(sample_v,ny*nc),rep(years_v, each = ns*nc)))
colnames(pre_1970_mdr) <- c("replicate", "year")   
pre_1970_mdr$iso3 <-rep(uu, each = ns)
pre_1970_mdr$value <- 0

pp_store$replicate <- pp_store$variable
all_samples <- rbind(pp_store[,c("replicate","year","iso3","value")], pre_1970_mdr)

#ggplot(all_samples, aes(x=year,  y = value, group = replicate)) + geom_line() + facet_wrap(~iso3)

all_samples_1970_2014 <-all_samples
write.csv(all_samples_1970_2014, "output_spline/all_samples_p_1970_2014.csv") 

## MERGE DS AND MDR
all0n <- merge(all_samples, rundatar, by = c("year","iso3","replicate"))
# MDR-ARI
all0n$mdr_ari <- all0n$value * all0n$ari

# TOTAL AND MDR
all0n$ds_ari <- all0n$ari - all0n$mdr_ari # ds ARI is the remainder of the ari

save(all0n,file="output_spline/all0n_p_ds_mdr.Rdata") 


### PLOT
# READ IN
#load("output_spline/all0n_p_ds_mdr.Rdata")
theme_set(theme_bw(base_size = 24))
pp <- "gaussian"
for(cci in 1:length(uu)){
  print(uu[cci])
  ### WHO data
  d <-subset(who0, iso3 == as.character(uu[cci]) )
  
  ### ARI for both DS and mDR in all0n
  rdata <- all0n[which(all0n$iso3 == as.character(uu[cci])),]
  
  a1 <- ggplot(d, aes(x=year_new, y = new_mdr_prop),col="red",pch = 10) + # points won't plot over lines unless do points first?!
    geom_point() +
    geom_line(data = rdata, aes(x=year, y = value, group = factor(replicate)),alpha = 0.2) +
    scale_y_continuous("Prop. new with MDR") + scale_x_continuous("Year",lim=c(1970,2016)) +
    geom_point(data = d, aes(x=year_new, y = new_mdr_prop),col="red",pch = 10, size = 3) + geom_errorbar(data = d, aes(ymin = mlo, ymax = mhi), col = "red")
  ggsave(paste0("output_spline/",uu[cci],"_mdr_trends_with_data_",pp,".pdf"),width=11, height=11)
  
  a2 <- ggplot(rdata, aes(x=year, y = mdr_ari, group = factor(replicate))) + geom_line(alpha = 0.2) +
    scale_y_continuous("MDR ARI") + scale_x_continuous("Year",lim=c(1970,2015))
  ggsave(paste0("output_spline/",uu[cci],"_mdr_ari_",pp,".pdf"),width=11, height=11)
  
}  


### New all0
load("../MDR-LTBI-paper/data_final/all0_p_ds_mdr.Rdata") # From running the code in Houben & Dodd in 2016.
w<-which(all0$iso3 %in% c("CHN","IND","USA"))
all0_new <- all0[-w,]
colnames(all0n)[which(colnames(all0n) == "value")] <- "prediction"
all0_new <- rbind(all0_new, all0n)

ggplot(subset(all0, iso3 %in% c("CHN","IND","USA")), aes(x=year, y = mdr_ari, group = replicate)) + geom_line() + facet_wrap(~iso3, scales = 'free') + 
  geom_line(data = subset(all0_new, iso3 %in% c("CHN","IND","USA")), aes(x=year, y = mdr_ari, group = replicate), col = "red", alpha = 0.2)  + ggtitle("MDR-ARI")
ggsave("output_spline/new_mdr_ARI_vs_old.pdf")       

ggplot(subset(all0, iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ds_ari, group = replicate)) + geom_line() + facet_wrap(~iso3, scales = "free") + 
  geom_line(data = subset(all0_new, iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ds_ari, group = replicate), col = "red", alpha = 0.2) + ggtitle("DS-ARI")
ggsave("output_spline/new_ds_ARI_vs_old.pdf")

ggplot(subset(all0, iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ari, group = replicate)) + geom_line() + facet_wrap(~iso3, scales = "free") + 
  geom_line(data = subset(all0_new, iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ari, group = replicate), col = "red", alpha = 0.2) + ggtitle("ARI")
ggsave("output_spline/new_ARI_vs_old.pdf")

## Restrict time
ggplot(subset(subset(all0, year > 1980), iso3 %in% c("CHN","IND","USA")), aes(x=year, y = mdr_ari, group = replicate)) + geom_line() + facet_wrap(~iso3, scales = 'free') + 
  geom_line(data = subset(subset(all0_new, year > 1980), iso3 %in% c("CHN","IND","USA")), aes(x=year, y = mdr_ari, group = replicate), col = "red", alpha = 0.2)  + ggtitle("MDR-ARI")
ggsave("output_spline/new_mdr_ARI_vs_old_recent.pdf")       

ggplot(subset(subset(all0, year > 1980), iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ds_ari, group = replicate)) + geom_line() + facet_wrap(~iso3, scales = "free") + 
  geom_line(data = subset(subset(all0_new, year > 1980), iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ds_ari, group = replicate), col = "red", alpha = 0.2) + ggtitle("DS-ARI")
ggsave("output_spline/new_ds_ARI_vs_old_recent.pdf")

ggplot(subset(subset(all0, year > 1980), iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ari, group = replicate)) + geom_line() + facet_wrap(~iso3, scales = "free") + 
  geom_line(data = subset(subset(all0_new, year > 1980), iso3 %in% c("CHN","IND","USA")), aes(x=year, y = ari, group = replicate), col = "red", alpha = 0.2) + ggtitle("ARI")
ggsave("output_spline/new_ARI_vs_old_recent.pdf")

# control... checking same. Yep! 
ggplot(subset(all0, iso3 %in% c("BRA","HKG","BWA")), aes(x=year, y = mdr_ari, group = replicate)) + geom_line() + facet_wrap(~iso3) +
  geom_line(data = subset(all0_new, iso3 %in% c("BRA","HKG","BWA")), aes(x=year, y = mdr_ari, group = replicate), col = "red")

## Save updated all0_new
save(all0_new,file="output_spline/all0new_p_ds_mdr.Rdata") 

