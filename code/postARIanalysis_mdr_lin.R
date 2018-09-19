## Analysis code for Houben & Dodd 2016, distributed under CC BY 4.0 license https://creativecommons.org/licenses/by/4.0/
# Modified for MDR
library(data.table)
library(reshape2)
library(dplyr)
library(ggplot2)
library(xtable)
theme_set(theme_bw(base_size = 24))

####**** Load data ********************************************************************************************************************************************************************#####
setwd("~/Documents/LTBI_MDR/")
load('data/whokey.Rdata')
load('data/All_3.Rdata')                          #ARI data
load('data/POP2014.Rdata')                  #population data
load('data/POP2035.Rdata')
load('data/POP2050.Rdata')
load('data/POP1997.Rdata')
load('data/DSN2.Rdata') #INH data from Dodd,Sismanidis,Seddon

final_list_cn <- read.csv("~/Dropbox/MDR/107_final_list_included_countries.csv", stringsAsFactors = FALSE)[,-1]
All <- All[All$lr_ari!=-Inf,] 

cnz <- final_list_cn #unique(as.character(All$iso3)) # included countries - accounts for too few entries 
length(cnz)

## need to have run GP regression first to generate this data (GPreg.R)
RUNZ <- BDZ <- list()
lin = 1 # for non-constant

for(i in 1:length(cnz)){ # for all the countries
    cn <- cnz[i]
    print(cn)
    
    if(lin > 0) {fn <- paste0('data/',cn,'.Rdata')}# grab the ARI data 
    if(lin == 0) {fn <- paste0('data_const/',cn,'.Rdata')}
    
    if(file.exists(fn)){
        load(fn)
        BDZ[[i]] <- erw
    }else{print(c(cn, "no file"))}
    
    if(lin > 0) {fn <- paste0('data/zz_',cn,'.Rdata')}# grab the ARI data 
    if(lin == 0) {fn <- paste0('data_const/zz_',cn,'.Rdata')}
    
    if(file.exists(fn)){
        load(fn)
        RUNZ[[i]] <- runsdf
    }
}

bdzdf <- do.call('rbind',BDZ)
rundatar <- do.call('rbind',RUNZ)
length(unique(rundatar$iso3)) # 107
save(rundatar,file='~/Dropbox/MDR/output/rundata_ari_1000.Rdata')

## ------------- analyse data -------------
rundatar <- data.table(rundatar)
rundr <- data.table(rundatar)

rundatar[,lari:=exp(lari)]               #now real ari!! 

## ADD IN MDR DATA
# Require mdr proportion for every year from 1934, for 1,000 replicates
all_samples0 <- read.csv("output/all_samples.csv")[,-1]
# Rename
all_samples <- all_samples0 %>%
  mutate(replicate = sample, iso3 = country) %>%
  dplyr::select(-sample) %>% dplyr::select(-country)
# Set negative to 0
all_samples[which(all_samples$prediction < 0), "prediction"] <- 0

# Add in 0 before 1970
ns <- max(all_samples0$sample)
sample_v <- seq(1,ns,1)
years <- unique(rundatar$year) 
years_v <- years[which(years < 1970)]
ny <- length(years_v)
nc <- length(final_list_cn)
pre_1970_mdr <- as.data.frame(cbind(rep(sample_v,ny*nc),rep(years_v, each = ns*nc)))
colnames(pre_1970_mdr) <- c("replicate", "year")   
pre_1970_mdr$iso3 <-rep(final_list_cn, each = ns)
pre_1970_mdr$prediction <- 0

all_samples <- rbind(all_samples, pre_1970_mdr)

all_samples_1970_2014 <-all_samples
write.csv(all_samples_1970_2014, "~/Dropbox/MDR/output/all_samples_1970_2014.csv")


#### Merge data together
# TB ARI from rundatar
# percentage new with MDR from all_samples
all0 <- merge(all_samples, rundatar, by = c("year","iso3","replicate"))

# mdr_ari
all0$mdr_ari <- all0$prediction * all0$lari

rall0 <- as.data.table(all0)

# CHECK
# ggplot(all[which(all$iso3 == "ALB"),], aes(x = year, y = mdr_ari, group = replicate)) + geom_line()
# ggplot(all[which(all$iso3 == "ALB"),], aes(x = year, y = mdr_ari, group = replicate)) + geom_line() + 
#   scale_x_continuous(lim = c(1980, 2018)) + scale_y_continuous(lim = c(0,0.001))
# ggplot(all[which(all$iso3 == "ALB"),], aes(x = year, y = lari, group = replicate)) + geom_line() + 
#   scale_x_continuous(lim = c(1980, 2018)) + scale_y_continuous(lim = c(0,1))

## rundatar
rall0[,year:=2014-year]               #now age

rall <- rall0[order(replicate,iso3,year),
                     list(ari=lari,mdr_ari=mdr_ari,
                          H=cumsum(lari),Hr=cumsum(mdr_ari),
                          year=year),
                     by=list(iso3=iso3,replicate=replicate)]

## ## for past 2 years
mask <- rep(1,length(unique(rall$year)))
mask[1:2] <- 0                          #all except last 2 years
rall[,dH:=cumsum(ari*mask),by=list(iso3=iso3,replicate=replicate)] #cumhaz!2y
rall[,dHr:=cumsum(mdr_ari*mask),by=list(iso3=iso3,replicate=replicate)] #cumhaz!2y
rall[,acat:=year %/% 5]

rall[,P:=1-exp(-H)]                  #ever, total TB
rall[,P1:=-exp(-H)+exp(-dH)]                  #1st recent=prob ever - prob not<2
rall[,Pr:=1-exp(-Hr)]                  #ever, MDR TB
rall[,P1r:=-exp(-Hr)+exp(-dHr)]                  #1st recent=prob ever - prob not<2


## CHANGE HERE SENSE!
## Andrews: 0.79 .7-.86 #### Protection from re-infecton
pm <- 0.79                              #0.5 #CHANGE HERE!
pv <- (0.86-0.7)^2/3.92^2
apb <- pm*(1-pm)/pv-1
pa <- pm*apb                            #77.88
pb <- (1-pm)*apb                        #20.70
## curve(dbeta(x,shape1 = pa,shape2=pb),from=0,to=1)
## abline(v=pm,col=2);abline(v=.86,col=2,lty=2);abline(v=.7,col=2,lty=2);
## swap
### Random sample of level of protection - beta distribution
alph <- rbeta(nrow(rall),shape1=pb,shape2=pa)

rall[,P2:=alph*(H-dH) + (1-alph)*(exp(-dH)-exp(-H))]                  #anyrecent
rall[,P2r:=alph*(Hr-dHr) + (1-alph)*(exp(-dHr)-exp(-Hr))]                  #anyrecent

rall <- rall[,list(P=mean(P),P1=mean(P1),P2=mean(P2),Pr=mean(Pr),P1r=mean(P1r),P2r=mean(P2r)),
             by=list(iso3,replicate,acat)]
rall <- merge(rall,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)


POP2014$age <- factor(POP2014$age)
POP2014$year <- as.numeric(POP2014$age)-1
colnames(POP2014)[4] <- 'pop'
colnames(POP2014)[5] <- 'acat'

rall <- merge(rall,POP2014[,list(iso3,acat,pop)],by=c('iso3','acat'),all.x=TRUE)

# ## drop NAs
# drpd <- as.character(rall[is.na(pop),unique(iso3)]) #population data mismatch - 24
# rall <- rall[!is.na(pop),]        #9K record the countries

# ## country size threshold 
# popsizes <- rall[replicate==1,list(pop=sum(pop)),by=iso3]
# popsizes[,sum(pop<5e2)]                 #24
# drp2 <- as.character(popsizes[pop<5e2,iso3])
# rall <- rall[!iso3 %in% drp2,]
# rall[,length(unique(iso3))]          #104
# length(unique(rall0$iso3))

# ## drop for having fewer than 15 datapoints
# tbl <- table(All$iso3)
# drp3 <- names(tbl)[which(tbl<15)][-1]   #4 countries
# 
# rall <- rall[!iso3 %in% drp3,]
# rall[,length(unique(iso3))]          #168

##  INH
DSN$INH <- DSN$INH + DSN$MDR         #include MDR with the INH-MR
DSN <- DSN[,c('iso3','iter','INH')]
DSN <- DSN[DSN$iso3 %in% as.character(unique(rall$iso3)),]
smp <- sample(1e3,1000)
DSN <- DSN[DSN$iter %in% smp,]            #sample 1000
DSN$replicate <- as.numeric(factor(DSN$iter))

## merge in INH data here
rall <- merge( rall, DSN[,-2], by=c('iso3','replicate'), all.x=TRUE)

## summary by age by region
rall_0 <- rall[,list(LTBI=sum(pop*P)/sum(pop),
                          LTBI1=sum(pop*P1)/sum(pop),
                          LTBI2=sum(pop*P2)/sum(pop),
                          LTBIH=sum(pop*P2*INH)/sum(pop),
              LTBIr=sum(pop*Pr)/sum(pop),
              LTBI1r=sum(pop*P1r)/sum(pop),
              LTBI2r=sum(pop*P2)/sum(pop)),
                    by=list(acat,replicate,g_whoregion)]


ub <- function(x)quantile(x,probs = .975)
lb <- function(x)quantile(x,probs = .025)
uq <- function(x)quantile(x,probs = .75)
lq <- function(x)quantile(x,probs = .25)

rall_0 <- rall_0[,list(LTBI=median(LTBI),LTBI1=median(LTBI1),
                           LTBI2=median(LTBI2), uq=uq(LTBI),lq=lq(LTBI),
                       LTBIr=median(LTBIr),LTBI1r=median(LTBI1r),
                       LTBI2r=median(LTBI2r), uqr=uq(LTBIr),lqr=lq(LTBIr)),
                     by=list(acat,g_whoregion)]

rall_0[,df:=LTBI2-LTBI1]
rall_0[,dfr:=LTBI2r-LTBI1r]

rall_0 <- melt(rall_0,id=c('acat','g_whoregion','uq','lq', 'uqr','lqr'))


acts <- paste0(5*(0:16),'-',5*(1:17))
acts[17] <- c('80-')
rall_0$age <- acts
rall_0$age <- factor(rall_0$age,levels=acts,ordered = TRUE)
## now examine prevalence by age


## look at country varations
rallc <- rall[,list(LTBI=sum(pop*P)/sum(pop),
                          LTBI1=sum(pop*P1)/sum(pop),
                          LTBI2=sum(pop*P2)/sum(pop),
                          LTBIH=sum(pop*P2*INH)/sum(pop),
                    LTBIr=sum(pop*Pr)/sum(pop),
                    LTBI1r=sum(pop*P1r)/sum(pop),
                    LTBI2r=sum(pop*P2r)/sum(pop),
                    LTBIHr=sum(pop*P2r*INH)/sum(pop)),
                    by=list(iso3,replicate)]

## for mapping
rallc <- rallc[,list(LTBI=median(LTBI),
                          LTBI1=median(LTBI1),
                          LTBI2=median(LTBI2),
                          LTBIH=median(LTBIH),
                     LTBIr=median(LTBIr),
                     LTBI1r=median(LTBI1r),
                     LTBI2r=median(LTBI2r),
                     LTBIHr=median(LTBIHr)),
                    by=list(iso3)]


## look at country varations
rallcT <- rall[,list(LTBI=sum(pop*P),
                          LTBI1=sum(pop*P1),
                          LTBI2=sum(pop*P2),
                          LTBIH=sum(pop*P2*INH),
                     LTBIr=sum(pop*Pr),
                     LTBI1r=sum(pop*P1r),
                     LTBI2r=sum(pop*P2r),
                     LTBIHr=sum(pop*P2r*INH)),
                    by=list(iso3,replicate)]

## for mapping
rallcT <- rallcT[,list(LTBI=median(LTBI),
                             LTBI_lo=quantile(LTBI,probs=0.025),
                             LTBI_hi=quantile(LTBI,probs=0.975),
                          LTBI1=median(LTBI1),
                          LTBI2=median(LTBI2),
                          LTBIH=median(LTBIH),
                       LTBIr=median(LTBIr),
                       LTBI_lor=quantile(LTBIr,probs=0.025),
                       LTBI_hir=quantile(LTBIr,probs=0.975),
                       LTBI1r=median(LTBI1r),
                       LTBI2r=median(LTBI2r),
                       LTBIHr=median(LTBIHr)),
                    by=list(iso3)]


top20c <- head(rallcT[order(LTBI,decreasing = TRUE),list(iso3,LTBI,LTBI_lo,LTBI_hi)],n=20)
top20cr <- head(rallcT[order(LTBIr,decreasing = TRUE),list(iso3,LTBIr,LTBI_lor,LTBI_hir)],n=20)
top20c$iso3 <- factor(top20c$iso3,levels=rev(top20c$iso3),ordered = TRUE)
top20cr$iso3 <- factor(top20cr$iso3,levels=rev(top20cr$iso3),ordered = TRUE)

top20c2 <- merge(top20c,rallc[,list(iso3,LTBI)],by='iso3',all.x=TRUE,all.y=FALSE)
top20c2 <- as.data.frame(top20c2)
names(top20c2)[c(2,5)] <- c('LTBI','Percent')
top20c2$Percent <- 1e2*top20c2$Percent

top20c2r <- merge(top20cr,rallc[,list(iso3,LTBIr)],by='iso3',all.x=TRUE,all.y=FALSE)
top20c2r <- as.data.frame(top20c2r)
names(top20c2r)[c(2,5)] <- c('LTBIr','Percent')
top20c2r$Percent <- 1e2*top20c2r$Percent


## ---------- numbers ------------
## -- by country --
rallic <- rall[,list(nLTBI=sum(pop*P),
                          nLTBI1=sum(pop*P1),
                          nLTBI2=sum(pop*P2),
                          nLTBIH=sum(pop*P2*INH),
                     nLTBIr=sum(pop*Pr),
                     nLTBI1r=sum(pop*P1r),
                     nLTBI2r=sum(pop*P2r),
                     nLTBIHr=sum(pop*P2r*INH)),
                    by=list(replicate,iso3)]

MID <- rallic[,list(nLTBI=median(nLTBI),
                          nLTBI1=median(nLTBI1),
                          nLTBI2=median(nLTBI2),
                          nLTBIH=median(nLTBIH),
                    nLTBIr=median(nLTBIr),
                    nLTBI1r=median(nLTBI1r),
                    nLTBI2r=median(nLTBI2r),
                    nLTBIHr=median(nLTBIHr)),
                    by=list(iso3)]
LO <- rallic[,list(nLTBI=lq(nLTBI),
                          nLTBI1=lq(nLTBI1),
                          nLTBI2=lq(nLTBI2),
                          nLTBIH=lq(nLTBIH),
                   nLTBIr=lq(nLTBIr),
                   nLTBI1r=lq(nLTBI1r),
                   nLTBI2r=lq(nLTBI2r),
                   nLTBIHr=lq(nLTBIHr)),
                    by=list(iso3)]
HI <- rallic[,list(nLTBI=uq(nLTBI),
                          nLTBI1=uq(nLTBI1),
                          nLTBI2=uq(nLTBI2),
                          nLTBIH=uq(nLTBIH),
                   nLTBIr=uq(nLTBIr),
                   nLTBI1r=uq(nLTBI1r),
                   nLTBI2r=uq(nLTBI2r),
                   nLTBIHr=uq(nLTBIHr)),
               by=list(iso3)]


## kids
rallck <- rall[acat<3,list(nLTBI=sum(pop*P),
                          nLTBI1=sum(pop*P1),
                          nLTBI2=sum(pop*P2),
                          nLTBIH=sum(pop*P2*INH),
                          nLTBIr=sum(pop*Pr),
                          nLTBI1r=sum(pop*P1r),
                          nLTBI2r=sum(pop*P2r),
                          nLTBIHr=sum(pop*P2r*INH)),
                    by=list(replicate,iso3)]

MIDk <- rallck[,list(nLTBI=median(nLTBI),
                          nLTBI1=median(nLTBI1),
                          nLTBI2=median(nLTBI2),
                          nLTBIH=median(nLTBIH),
                     nLTBIr=median(nLTBIr),
                     nLTBI1r=median(nLTBI1r),
                     nLTBI2r=median(nLTBI2r),
                     nLTBIHr=median(nLTBIHr)),
                    by=list(iso3)]
LOk <- rallck[,list(nLTBI=lq(nLTBI),
                          nLTBI1=lq(nLTBI1),
                          nLTBI2=lq(nLTBI2),
                          nLTBIH=lq(nLTBIH),
                    nLTBIr=lq(nLTBIr),
                    nLTBI1r=lq(nLTBI1r),
                    nLTBI2r=lq(nLTBI2r),
                    nLTBIHr=lq(nLTBIHr)),
                    by=list(iso3)]
HIk <- rallck[,list(nLTBI=uq(nLTBI),
                          nLTBI1=uq(nLTBI1),
                          nLTBI2=uq(nLTBI2),
                          nLTBIH=uq(nLTBIH),
                         nLTBIr=uq(nLTBIr),
                         nLTBI1r=uq(nLTBI1r),
                         nLTBI2r=uq(nLTBI2r),
                         nLTBIHr=uq(nLTBIHr)),
               by=list(iso3)]


MID <- as.data.frame(MID);LO <- as.data.frame(LO);HI <- as.data.frame(HI);
MID$K <- MIDk$nLTBI; LO$K <- LOk$nLTBI; HI$K <- HIk$nLTBI
MID$Kr <- MIDk$nLTBIr; LO$Kr <- LOk$nLTBIr; HI$Kr <- HIk$nLTBIr



## -- by region --
## total numbs
rall1 <- rall[,list(nLTBI=sum(pop*P),
                          nLTBI1=sum(pop*P1),
                          nLTBI2=sum(pop*P2),
                          nLTBIH=sum(pop*P2*INH),
                          nLTBIr=sum(pop*Pr),
                          nLTBI1r=sum(pop*P1r),
                          nLTBI2r=sum(pop*P2r),
                          nLTBIHr=sum(pop*P2r*INH)),
                    by=list(replicate,g_whoregion)]
rundatar1g <- rall[,list(nLTBI=sum(pop*P),
                          nLTBI1=sum(pop*P1),
                          nLTBI2=sum(pop*P2),
                          nLTBIH=sum(pop*P2*INH),
                         nLTBIr=sum(pop*Pr),
                         nLTBI1r=sum(pop*P1r),
                         nLTBI2r=sum(pop*P2r),
                         nLTBIHr=sum(pop*P2r*INH)),
                    by=list(replicate)]
MID <- rall1[,list(nLTBI=median(nLTBI),
                          nLTBI1=median(nLTBI1),
                          nLTBI2=median(nLTBI2),
                          nLTBIH=median(nLTBIH),
                  nLTBIr=median(nLTBIr),
                  nLTBI1r=median(nLTBI1r),
                  nLTBI2r=median(nLTBI2r),
                  nLTBIHr=median(nLTBIHr)),
                    by=list(g_whoregion)]
LO <- rall1[,list(nLTBI=lb(nLTBI),
                          nLTBI1=lb(nLTBI1),
                          nLTBI2=lb(nLTBI2),
                          nLTBIH=lb(nLTBIH),
                      nLTBIr=lb(nLTBIr),
                      nLTBI1r=lb(nLTBI1r),
                      nLTBI2r=lb(nLTBI2r),
                      nLTBIHr=lb(nLTBIHr)),
                    by=list(g_whoregion)]
HI <- rall1[,list(nLTBI=ub(nLTBI),
                          nLTBI1=ub(nLTBI1),
                          nLTBI2=ub(nLTBI2),
                          nLTBIH=ub(nLTBIH),
                      nLTBIr=ub(nLTBIr),
                      nLTBI1r=ub(nLTBI1r),
                      nLTBI2r=ub(nLTBI2r),
                      nLTBIHr=ub(nLTBIHr)),
               by=list(g_whoregion)]

## kids
rallk <- rall[acat<3,list(nLTBI=sum(pop*P),
                          nLTBI1=sum(pop*P1),
                          nLTBI2=sum(pop*P2),
                          nLTBIH=sum(pop*P2*INH),
                          nLTBIr=sum(pop*Pr),
                          nLTBI1r=sum(pop*P1r),
                          nLTBI2r=sum(pop*P2r),
                          nLTBIHr=sum(pop*P2r*INH)),
                    by=list(replicate,g_whoregion)]
rundatar1gk <- rall[acat<3,list(nLTBI=sum(pop*P),
                          nLTBI1=sum(pop*P1),
                          nLTBI2=sum(pop*P2),
                          nLTBIH=sum(pop*P2*INH),
                          nLTBIr=sum(pop*Pr),
                          nLTBI1r=sum(pop*P1r),
                          nLTBI2r=sum(pop*P2r),
                          nLTBIHr=sum(pop*P2r*INH)),
                    by=list(replicate)]
MIDk <- rallk[,list(nLTBI=median(nLTBI),
                          nLTBI1=median(nLTBI1),
                          nLTBI2=median(nLTBI2),
                          nLTBIH=median(nLTBIH),
                    nLTBIr=median(nLTBIr),
                    nLTBI1r=median(nLTBI1r),
                    nLTBI2r=median(nLTBI2r),
                    nLTBIHr=median(nLTBIHr)),
                    by=list(g_whoregion)]
LOk <- rallk[,list(nLTBI=lb(nLTBI),
                          nLTBI1=lb(nLTBI1),
                          nLTBI2=lb(nLTBI2),
                          nLTBIH=lb(nLTBIH),
                   nLTBIr=lb(nLTBIr),
                   nLTBI1r=lb(nLTBI1r),
                   nLTBI2r=lb(nLTBI2r),
                   nLTBIHr=lb(nLTBIHr)),
                    by=list(g_whoregion)]
HIk <- rallk[,list(nLTBI=ub(nLTBI),
                          nLTBI1=ub(nLTBI1),
                          nLTBI2=ub(nLTBI2),
                          nLTBIH=ub(nLTBIH),
                   nLTBIr=ub(nLTBIr),
                   nLTBI1r=ub(nLTBI1r),
                   nLTBI2r=ub(nLTBI2r),
                   nLTBIHr=ub(nLTBIHr)),
                    by=list(g_whoregion)]

## all
MID <- as.data.frame(MID);LO <- as.data.frame(LO);HI <- as.data.frame(HI);
MIDg <- rundatar1g[,list(nLTBI=median(nLTBI),
                          nLTBI1=median(nLTBI1),
                          nLTBI2=median(nLTBI2),
                          nLTBIH=median(nLTBIH),
                         nLTBIr=median(nLTBIr),
                         nLTBI1r=median(nLTBI1r),
                         nLTBI2r=median(nLTBI2r),
                         nLTBIHr=median(nLTBIHr))]
LOg <- rundatar1g[,list(nLTBI=lb(nLTBI),
                          nLTBI1=lb(nLTBI1),
                          nLTBI2=lb(nLTBI2),
                          nLTBIH=lb(nLTBIH),
                        nLTBIr=lb(nLTBIr),
                        nLTBI1r=lb(nLTBI1r),
                        nLTBI2r=lb(nLTBI2r),
                        nLTBIHr=lb(nLTBIHr))]
HIg <- rundatar1g[,list(nLTBI=ub(nLTBI),
                          nLTBI1=ub(nLTBI1),
                          nLTBI2=ub(nLTBI2),
                          nLTBIH=ub(nLTBIH),
                        nLTBIr=ub(nLTBIr),
                        nLTBI1r=ub(nLTBI1r),
                        nLTBI2r=ub(nLTBI2r),
                        nLTBIHr=ub(nLTBIHr))]
MIDg <- as.data.frame(MIDg);LOg <- as.data.frame(LOg);HIg <- as.data.frame(HIg);
MIDg <- cbind(g_whoregion='GLOBAL',MIDg);LOg <- cbind(g_whoregion='GLOBAL',LOg);
HIg <- cbind(g_whoregion='GLOBAL',HIg)
MID <- rbind(MID,MIDg);HI <- rbind(HI,HIg);LO <- rbind(LO,LOg)



## kids
MID0 <- MID
MID <- cbind(MID, c(MIDk[,nLTBI],rundatar1gk[,median(nLTBI)] ))
MID <- cbind(MID, c(MIDk[,nLTBIr],rundatar1gk[,median(nLTBIr)] ))
LO <- cbind(LO, c(LOk[,nLTBI],rundatar1gk[,lb(nLTBI)] ))
LO <- cbind(LO, c(LOk[,nLTBIr],rundatar1gk[,lb(nLTBIr)] ))
HI <- cbind(HI, c(HIk[,nLTBI],rundatar1gk[,ub(nLTBI)] ))
HI <- cbind(HI, c(HIk[,nLTBIr],rundatar1gk[,ub(nLTBIr)] ))
names(MID)[10] <- names(LO)[10] <- names(HI)[10] <- 'nkLTBI'
names(MID)[11] <- names(LO)[11] <- names(HI)[11] <- 'nkLTBIr'

MIDn <- MID; LOn <- LO; HIn <- HI 

### DIDN"T DO R FOR INH - not quite worked out what means
# ## ---  for INH proportions ---
# rundatR <- rundatar1[,list(propRH=nLTBIH/nLTBI2),by=list(replicate,g_whoregion)]
# rundatRg <- rundatar1g[,list(propRH=nLTBIH/nLTBI2),by=list(replicate)]
# 
# MIDR <- rundatR[,list(mid=median(propRH)),by=g_whoregion]
# LOR <- rundatR[,list(lo=lb(propRH)),by=g_whoregion]
# HIR <- rundatR[,list(hi=ub(propRH)),by=g_whoregion]
# 
# MIDR <- as.data.frame(MIDR);LOR <- as.data.frame(LOR);HIR <- as.data.frame(HIR);
# MIDR <- rbind(MIDR,data.frame(g_whoregion='GLOBAL',mid=median(rundatRg[,propRH])))
# LOR <- rbind(LOR,data.frame(g_whoregion='GLOBAL',lo=lb(rundatRg[,propRH])))
# HIR <- rbind(HIR,data.frame(g_whoregion='GLOBAL',hi=ub(rundatRg[,propRH])))
 

## ---------- props ------------
## total props
rall2 <- rall[,list(LTBI=sum(pop*P)/sum(pop),
                          LTBI1=sum(pop*P1)/sum(pop),
                          LTBI2=sum(pop*P2)/sum(pop),
                          LTBIH=sum(pop*P2*INH)/sum(pop),
                        LTBIr=sum(pop*Pr)/sum(pop),
                        LTBI1r=sum(pop*P1r)/sum(pop),
                        LTBI2r=sum(pop*P2r)/sum(pop),
                        LTBIHr=sum(pop*P2r*INH)/sum(pop)),
                    by=list(replicate,g_whoregion)]
rall2g <- rall[,list(LTBI=sum(pop*P)/sum(pop),
                          LTBI1=sum(pop*P1)/sum(pop),
                          LTBI2=sum(pop*P2)/sum(pop),
                          LTBIH=sum(pop*P2*INH)/sum(pop),
                     LTBIr=sum(pop*Pr)/sum(pop),
                     LTBI1r=sum(pop*P1r)/sum(pop),
                     LTBI2r=sum(pop*P2r)/sum(pop),
                     LTBIHr=sum(pop*P2r*INH)/sum(pop)),
                    by=list(replicate)]


## prop in kids
rall2[,propk:=rallk[,nLTBI]/rall1[,nLTBI]]
rall2g[,propk:=rundatar1gk[,nLTBI]/rundatar1g[,nLTBI]]

rall2[,propkr:=rallk[,nLTBIr]/rall1[,nLTBIr]]
rall2g[,propkr:=rundatar1gk[,nLTBIr]/rundatar1g[,nLTBIr]]


MID <- rall2[,list(LTBI=median(LTBI),
                      LTBI1=median(LTBI1),
                      LTBI2=median(LTBI2),
                      LTBIH=median(LTBIH),
                      propk=median(propk),
                   LTBIr=median(LTBIr),
                    LTBI1r=median(LTBI1r),
                    LTBI2r=median(LTBI2r),
                    LTBIHr=median(LTBIHr),
                    propkr=median(propkr)),
                    by=list(g_whoregion)]
LO <- rall2[,list(LTBI=lb(LTBI),
                     LTBI1=lb(LTBI1),
                     LTBI2=lb(LTBI2),
                     LTBIH=lb(LTBIH),
                     propk=lb(propk),
                     LTBIr=lb(LTBIr),
                     LTBI1r=lb(LTBI1r),
                     LTBI2r=lb(LTBI2r),
                     LTBIHr=lb(LTBIHr),
                     propkr=lb(propkr)),
                    by=list(g_whoregion)]
HI <- rall2[,list(LTBI=ub(LTBI),
                     LTBI1=ub(LTBI1),
                     LTBI2=ub(LTBI2),
                     LTBIH=ub(LTBIH),
                     propk=ub(propk),
                  LTBIr=ub(LTBIr),
                  LTBI1r=ub(LTBI1r),
                  LTBI2r=ub(LTBI2r),
                  LTBIHr=ub(LTBIHr),
                  propkr=ub(propkr)),
                    by=list(g_whoregion)]

MID <- as.data.frame(MID);LO <- as.data.frame(LO);HI <- as.data.frame(HI);
MIDg <- rall2g[,list(LTBI=median(LTBI),
                        LTBI1=median(LTBI1),
                        LTBI2=median(LTBI2),
                        LTBIH=median(LTBIH),
                        propk=median(propk),
                        LTBIr=median(LTBIr),
                        LTBI1r=median(LTBI1r),
                        LTBI2r=median(LTBI2r),
                        LTBIHr=median(LTBIHr),
                        propkr=median(propkr))]
LOg <- rall2g[,list(LTBI=lb(LTBI),
                       LTBI1=lb(LTBI1),
                       LTBI2=lb(LTBI2),
                       LTBIH=lb(LTBIH),
                       propk=lb(propk),
                    LTBIr=lb(LTBIr),
                    LTBI1r=lb(LTBI1r),
                    LTBI2r=lb(LTBI2r),
                    LTBIHr=lb(LTBIHr),
                    propkr=lb(propkr))]
HIg <- rall2g[,list(LTBI=ub(LTBI),
                       LTBI1=ub(LTBI1),
                       LTBI2=ub(LTBI2),
                       LTBIH=ub(LTBIH),
                       propk=ub(propk),
                    LTBIr=ub(LTBIr),
                    LTBI1r=ub(LTBI1r),
                    LTBI2r=ub(LTBI2r),
                    LTBIHr=ub(LTBIHr),
                    propkr=ub(propkr))]
MIDg <- as.data.frame(MIDg);LOg <- as.data.frame(LOg);HIg <- as.data.frame(HIg);
MIDg <- cbind(g_whoregion='GLOBAL',MIDg);LOg <- cbind(g_whoregion='GLOBAL',LOg);
HIg <- cbind(g_whoregion='GLOBAL',HIg)
MID <- rbind(MID,MIDg);HI <- rbind(HI,HIg);LO <- rbind(LO,LOg)

## per mille
LO$LTBIH <- LO$LTBIH*10; MID$LTBIH <- MID$LTBIH*10; HI$LTBIH <- HI$LTBIH*10
LO$LTBIHr <- LO$LTBIHr*10; MID$LTBIHr <- MID$LTBIHr*10; HI$LTBIHr <- HI$LTBIHr*10


#### STOPPED R HERE FOR NOW !!!!!!!!
####
#####

## -------- 2035 & 2050 -----------


N2035 <- POP2035[,sum(value)]
POP2035$acatn <- 0:16
POP2035$acat <- POP2035$acatn-4
POP2035 <- POP2035[acat>=0,]
POP2035 <- POP2035[,list(iso3,acat,pop35=value)]
N2050 <- POP2050[,sum(value)]
POP2050$acatn <- 0:16
POP2050$acat <- POP2050$acatn-7
POP2050 <- POP2050[acat>=0,]
POP2050 <- POP2050[,list(iso3,acat,pop50=value)]

fac <- 1e5*(0.15*1e-2)                  #reactivation rate

## 2035
fruns <- merge(rundatar[,list(iso3,replicate,acat,P,g_whoregion)],
               POP2035,by=c('iso3','acat'),all=TRUE)
fruns <- fruns[!is.na(pop35),]       #ditch the dead
tmp <- fruns[,list(nLTBI=sum(pop35*P)),by=replicate]
tmp <- tmp[!is.na(replicate)]
tmp[,list(mid=1e3*median(nLTBI),lo=1e3*lb(nLTBI),hi=1e3*ub(nLTBI))]


fac*tmp[,list(mid=median(nLTBI)/N2035,lo=lb(nLTBI)/N2035,hi=ub(nLTBI)/N2035)]

## 2050
fruns <- merge(rundatar[,list(iso3,replicate,acat,P,g_whoregion)],
               POP2050,by=c('iso3','acat'),all=TRUE)
fruns <- fruns[!is.na(pop50),]       #ditch the dead
tmp <- fruns[,list(nLTBI=sum(pop50*P)),by=replicate]
tmp <- tmp[!is.na(replicate)]
tmp[,list(mid=1e3*median(nLTBI),lo=1e3*lb(nLTBI),hi=1e3*ub(nLTBI))]

fac*tmp[,list(mid=median(nLTBI)/N2050,lo=lb(nLTBI)/N2050,hi=ub(nLTBI)/N2050)]

## 1997 --- NB rundatar not available after this

rund[,year:=1997-year]               #now age
rund[,lari:=exp(lari)]               #now real ari
rund <- rund[year>=0,]               #ditch future
rund <- rund[iso3 %in% unique(rundatar$iso3)] #droppings

## past
past <- data.frame(year=64:80,iso3=rep(unique(rund$iso3),each=length(64:80)),
                   lari=NA, replicate =
                       rep(1:200,each=length(64:80)*length(unique(rund$iso3)) ))
past <- as.data.table(past)
pastlari <- rund[year==63,list(lari),by=list(iso3,replicate)]
past <- merge(past,pastlari,all.x=TRUE,by=c('iso3','replicate'))
past[,lari:=lari.y]
past <- past[,list(iso3,replicate,year,lari)]

rund <- merge(rund,past,all=TRUE,by=c('iso3','year','replicate'))
rund[,lari:=pmax(lari.x,lari.y,na.rm=TRUE)]
rund <- rund[,list(iso3,replicate,year,lari)]
rund <- rund[order(replicate,iso3,year),]
rund <- rund[,list(ari=lari,H=cumsum(lari),year=year),by=list(iso3=iso3,replicate=replicate)]
rund[,acat:=year %/% 5]
rund[,P:=1-exp(-H)]                  #ever
rund <- rund[,list(P=mean(P)),
                   by=list(iso3,replicate,acat)]
rund <- merge(rund,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE)

## demographic data

N1997 <- POP1997[,sum(value)]
POP1997$acat <- 0:16
POP1997 <- POP1997[,list(iso3,acat,pop97=value)]
## 93% younger than 65

pruns <- merge(rund[,list(iso3,replicate,acat,P,g_whoregion)],
               POP1997,by=c('iso3','acat'),all.x=TRUE)
## pruns[,sum(pop97)/200]*1e-6
pruns <- pruns[!is.na(P),]

tmp <- pruns[,list(nLTBI=sum(pop97*P)),by=replicate]
tmp[,list(mid=1e3*median(nLTBI),lo=1e3*lb(nLTBI),hi=1e3*ub(nLTBI))]*1e-6

tmp <- pruns[,list(pLTBI=sum(pop97*P)/sum(pop97)),by=replicate]
tmp[,list(mid=median(pLTBI),lo=lb(pLTBI),hi=ub(pLTBI))]

########***** OUTPUT ******#######
order <- c(4,2,6,5,3,1,7) # to match H&D


table1 <- as.data.frame(cbind( paste0(sprintf('%.1f',100*MID$LTBI), " [", 
                       sprintf('%.1f',100*LO$LTBI), "-", 
                       sprintf('%.1f',100*HI$LTBI),"]"),
                 paste0(sprintf('%.1f',100*MID$propk), " [", 
                        sprintf('%.1f',100*LO$propk), "-", 
                        sprintf('%.1f',100*HI$propk),"]"),
                 paste0(sprintf('%.2f',100*MID$LTBIr), " [", 
                        sprintf('%.2f',100*LO$LTBIr), "-", 
                        sprintf('%.2f',100*HI$LTBIr),"]")
                 ))
colnames(table1) <- c("Prev_LTBI(%)","Prop_in_kids(%)","Prev_MDR-LTBI(%)")

table1$WHOr <-MID$g_whoregion 

table1 <- table1[order,]
table1

xtable(table1[,c("WHOr","Prev_LTBI(%)","Prev_MDR-LTBI(%)")])

write.csv(table1, "~/Dropbox/MDR/output/table1_lin.csv")
table1 <- read.csv("~/Dropbox/MDR/output/table1_lin.csv")[,-1]

table1n <- as.data.frame(cbind( paste0(sprintf('%.1f',100*MIDn$nLTBI), " [", 
                                      sprintf('%.1f',100*LOn$nLTBI), "-", 
                                      sprintf('%.1f',100*HIn$nLTBI),"]"),
                               paste0(sprintf('%.1f',100*MIDn$nkLTBI), " [", 
                                      sprintf('%.1f',100*LOn$nkLTBI), "-", 
                                      sprintf('%.1f',100*HIn$nkLTBI),"]"),
                               paste0(sprintf('%.2f',100*MIDn$nLTBIr), " [", 
                                      sprintf('%.2f',100*LOn$nLTBIr), "-", 
                                      sprintf('%.2f',100*HIn$nLTBIr),"]"),
                               paste0(sprintf('%.2f',100*MIDn$nkLTBIr), " [", 
                                      sprintf('%.2f',100*LOn$nkLTBIr), "-", 
                                      sprintf('%.2f',100*HIn$nkLTBIr),"]")
))
colnames(table1n) <- c("n_LTBI","nProp_in_kids","n_MDR-LTBI","nMDRProp_in_kids")

table1n$WHOr <-MID$g_whoregion 

table1n <- table1n[order,]
table1n

write.csv(table1n, "~/Dropbox/MDR/output/table1n_lin.csv")


########***** OUTPUT ******#######
order <- c(4,2,6,5,3,1,7) # to match H&D

LOn <- LO[,c("LTBI", "LTBIr")]
colnames(LOn) <- c("dslo", "mdrlo")
HIn <- HI[,c("LTBI", "LTBIr")]
colnames(HIn) <- c("dshi", "mdrhi")

tt1 <- cbind(MID[,c("g_whoregion","LTBI", "LTBIr")], LOn, HIn) 

tt1m <- melt(MID[,c("g_whoregion","LTBI", "LTBIr")], id.vars = c("g_whoregion"))
tt1m <- cbind(tt1m, cbind(c(LOn$dslo, LOn$mdrlo), c(HIn$dshi,HIn$mdrhi)))
colnames(tt1m) <- c("g_whoregion","variable","value","lo","hi")
tt1m$global <- 0
tt1m[which(tt1m$g_whoregion == "GLOBAL"),"global"] <- 1

ggplot(tt1m, aes(x=g_whoregion, y = 100*value, fill = factor(variable),colour = factor(global))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(g_whoregion,X = value*(variable == "LTBI"))) +
  scale_fill_manual("LTBI", labels = c("DS","MDR"), values = c("blue","red")) + 
  scale_x_discrete("WHO region") + scale_y_continuous("Percentage of population") + 
  geom_errorbar(aes(ymin=100*lo, ymax=100*hi), width=.2,position=position_dodge(.9)) + 
  scale_color_manual(values = c("red","black")) + 
  guides(colour = FALSE)
ggsave("~/Dropbox/MDR/output/LTBIDSMDR.pdf")

ggplot(tt1m, aes(x=g_whoregion, y = 100*value, fill = factor(variable),colour = factor(global))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(g_whoregion,X = value*(variable == "LTBIr"))) +
  scale_fill_discrete("LTBI", labels = c("DS","MDR")) + 
  scale_x_discrete("WHO region") + scale_y_continuous("Percentage of population") + 
  geom_errorbar(aes(ymin=100*lo, ymax=100*hi), width=.2,position=position_dodge(.9)) + 
  scale_color_manual(values = c("red","black")) + 
  guides(colour = FALSE)
  
ggplot(subset(tt1m, variable == "LTBIr"), 
       aes(x=g_whoregion, y = 100*value, fill = factor(variable), colour = factor(global))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(g_whoregion,X = value*(variable == "LTBIr"))) +
  scale_fill_discrete("LTBI", labels = c("MDR")) + 
  scale_x_discrete("WHO region") + scale_y_continuous("Percentage of population") + 
  geom_errorbar(aes(ymin=100*lo, ymax=100*hi), width=.2,position=position_dodge(.9)) + 
  scale_color_manual(values = c("red","black")) + 
  guides(colour = FALSE)
ggsave("~/Dropbox/MDR/output/LTBIMDR.pdf")


ggplot(subset(ms_mean, variable == "ltbis"), aes(x=pop_name, y = value, fill = factor(rep))) + 
  geom_bar(position = position_dodge(width = 0.5),stat="identity") + 
  coord_flip() + aes(x=reorder(pop_name,X = value*(variable == "ltbis"))) + 
  scale_fill_discrete("MDR\nARI\ntrend") + 
  scale_x_discrete("Country") + scale_y_continuous("Percentage of population") +
  theme(axis.text.y = element_text(colour="grey20",size=6))

table1 <- as.data.frame(cbind( paste0(sprintf('%.1f',100*MID$LTBI), " [", 
                                      sprintf('%.1f',100*LO$LTBI), "-", 
                                      sprintf('%.1f',100*HI$LTBI),"]"),
                               paste0(sprintf('%.1f',100*MID$propk), " [", 
                                      sprintf('%.1f',100*LO$propk), "-", 
                                      sprintf('%.1f',100*HI$propk),"]"),
                               paste0(sprintf('%.2f',100*MID$LTBIr), " [", 
                                      sprintf('%.2f',100*LO$LTBIr), "-", 
                                      sprintf('%.2f',100*HI$LTBIr),"]")
))
colnames(table1) <- c("Prev_LTBI(%)","Prop_in_kids(%)","Prev_MDR-LTBI(%)")

table1$WHOr <-MID$g_whoregion 

