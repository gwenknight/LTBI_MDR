### Which countries included?

### Original WHO data on MDR
w_data <- read.csv("~/Documents/LTBI_MDR/datar/new_who_edited.csv", stringsAsFactors = FALSE)
u <- unique(w_data$iso3)
length(unique(w_data$iso3)) # 159

### From H&D:168 countries included (see appendix for reasons)
load("~/Documents/LTBI_MDR/datar_approx/uu.Rdata") 
length(uu)
uu

### Intersection of the two?
length(intersect(uu,u)) # 138
length(setdiff(uu,u)) # 30
length(setdiff(u,uu)) # 21

setdiff(uu,u)

setdiff(u,uu)

# new <- c(intersect(uu,u),setdiff(u,uu))
# setdiff(u,new)
# setdiff(new,u)


## SCG = Serbia and Montenegro prior to split in 2005


### Pop size in 2014
load("/Users/macbook_IC/Documents/LTBI_MDR/data/POP2014.Rdata")
inlnw <- setdiff(uu,u) # in latent not MDR - what percentage of global population?

# have to aggregate over age groups for each country
apop <- aggregate(POP2014$value, by=list(iso3=POP2014$iso3), FUN=sum) # in 1,000s
totalp14 <- sum(apop$x)
# what population size are the countries in latent but not WHO MDR
wm <- match(inlnw, apop$iso3)
psizewm <- sum(apop[wm,"x"])
perc_wm <- 100*psizewm / totalp14 # 4.67% 
perc_wm

### TB burden 2014
whotb <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/TB_burden_countries_2018-08-10.csv")
whotb2014 <- subset(whotb,whotb$year == 2014)
utb <- unique(whotb2014$iso3)
length(utb)

setdiff(u,utb) # none in WHO MDR that aren't in here = correct
setdiff(uu,utb) # none in LTBI that aren't in here = correct
length(setdiff(utb,u)) # 57 in here that aren't in WHO MDR

totaltb14 <- sum(whotb2014$e_inc_100k)

# what TB contribution are the countries in latent but not WHO MDR
wm <- match(inlnw, whotb2014$iso3)
tbsizewm <- sum(whotb2014[wm,"e_inc_100k"])
perc_wm <- 100*tbsizewm / totaltb14 # 20% 
perc_wm
round(100*whotb2014[wm,"e_inc_100k"]/ totaltb14,2) # all contribute < 2% individually

## Top 30 HBC MDR
mdr30 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/top30_mdr_countries.csv")

### need to add in iso3 - DONE
#mdr30 <- merge(mdr30,whotb[,c("country","iso3")], by = "country")
#indexmdr<-as.numeric(rownames(unique(data.frame(mdr30$iso3)[1]))) 
#mdr30 <- mdr30[indexmdr,]
#write.csv(mdr30,"mdr30.csv")

# MDR TB burden 2018
mtb18 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/MDR_RR_TB_burden_estimates_2018-08-10.csv", stringsAsFactors = FALSE)
mtb18$mdr_inc_num <- mtb18$mdr_inc_num/1000

totalmtb18 <- sum(mtb18$mdr_inc_num) # 493804 = similar to 2017 GTB report
mtb18$perc_total <- 100*mtb18$mdr_inc_num / totalmtb18

# what MDRTB contribution are the countries in latent but not WHO MDR
totalmtb14 <- 490 # GLOBAL TB report 2017

wm <- match(inlnw, mtb18$iso3, nomatch = 0)
perc_wm <- sum(mtb18[wm,"perc_total"]) # 3.6% 
perc_wm
round(mtb18[wm,"perc_total"],2) 

### Intersecting list Latent and WHO MDR
final_list <- intersect(uu,u)
wm <- match(final_list, mtb18$iso3, nomatch = 0)
perc_wm <- sum(mtb18[wm,"perc_total"]) # 96.4%
perc_wm
round(mtb18[wm,"perc_total"],2) # two contribute 0.9 and 1.5% individually

# only Angola missing?!
w<-which(mdr30$iso3 == "AGO")
100*mdr30[w,"mdr_inc_num"]/totalmtb14

### Remove those with only 1 MDR datapoint
mdr_cn_best <- read.csv("~/Documents/LTBI_MDR/output/store_cn_best.csv", stringsAsFactors = FALSE)[-1]

wm <- match(final_list,mdr_cn_best$country, nomatch = 0)
n_datapoints <- mdr_cn_best[wm,c("country","n_data")]
dim(subset(n_datapoints, n_data < 2)) # 31 only have 1 datapoint
rem_1_dp <- subset(n_datapoints, n_data < 2)[,1]

# remove these from the final_list
final_list <- setdiff(final_list, rem_1_dp)
length(final_list) # 107

#### FINAL LIST
wm <- match(final_list, mtb18$iso3, nomatch = 0)
perc_wm <- sum(mtb18[wm,"perc_total"]) # 82.8% 
perc_wm
round(mtb18[wm,"perc_total"],2) #
max(round(mtb18[wm,"perc_total"],2)) # one contributes 26% = CHINA

wmm <- match(rem_1_dp, mtb18$iso3, nomatch = 0)
round(mtb18[wmm,"perc_total"],2) # one contributes 2.5% = Nigeria. 5.1% = Pakistan
sum(100*mtb18[wmm,"mdr_inc_num"]/ totalmtb18) # 14% removed as only one point
mtb18[wmm,c("country","mdr_inc_num","perc_total")]
