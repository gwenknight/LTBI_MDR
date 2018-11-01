### Which countries included?
library(magrittr)
library(dplyr)
library(ggplot2)

### Original WHO data on MDR
w_data <- read.csv("~/Dropbox/MDR/who/new_who_edited.csv",stringsAsFactors = FALSE)[,-1]
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

## SCG = Serbia and Montenegro prior to split in 2005 - already chagned in new_who_edited


### Pop size in 2014
load("~/Documents/LTBI_MDR/data/POP2014.Rdata")
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

totaltb14 <- sum(whotb2014$e_inc_num)

# what TB contribution are the countries in latent but not WHO MDR
wm <- match(inlnw, whotb2014$iso3)
tbsizewm <- sum(whotb2014[wm,"e_inc_num"])
perc_wm <- 100*tbsizewm / totaltb14 # 6% 
perc_wm
round(100*whotb2014[wm,"e_inc_num"]/ totaltb14,2) # all contribute < 1% individually except 14 COD = 2.3%

## Top 30 HBC MDR
mdr30 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/top30_mdr_countries.csv")

### need to add in iso3 - DONE
#mdr30 <- merge(mdr30,whotb[,c("country","iso3")], by = "country")
#indexmdr<-as.numeric(rownames(unique(data.frame(mdr30$iso3)[1]))) 
#mdr30 <- mdr30[indexmdr,]
#write.csv(mdr30,"mdr30.csv")

# MDR TB burden 2016
mtb18 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/WHO_data/MDR_RR_TB_burden_estimates_2018-08-10.csv", stringsAsFactors = FALSE)
mtb18$mdr_inc_num <- mtb18$mdr_inc_num/1000

totalmtb18 <- sum(mtb18$mdr_inc_num) # 493804 = similar to 2017 GTB report
mtb18$perc_total <- 100*mtb18$mdr_inc_num / totalmtb18

# what MDRTB contribution are the countries in latent but not WHO MDR
totalmtb14 <- 458 # GLOBAL TB report 2018 PAGE 27

wm <- match(inlnw, mtb18$iso3, nomatch = 0)
perc_wm <- sum(mtb18[wm,"perc_total"]) # 3.6% 
perc_wm
round(mtb18[wm,"perc_total"],2) 

### Intersecting list Latent and WHO MDR
final_list <- intersect(uu,u)
wm <- match(final_list, mtb18$iso3, nomatch = 0)
perc_wm <- sum(mtb18[wm,"perc_total"]) # 96.4% of incident MDR-TB cases 
perc_wm
round(mtb18[wm,"perc_total"],2) # two contribute 0.9 and 1.5% individually

# only Angola and COD missing?!
w<-which(mdr30$iso3 == "AGO")
100*mdr30[w,"mdr_inc_num"]/totalmtb14

w<-which(mdr30$iso3 == "COD")
100*mdr30[w,"mdr_inc_num"]/totalmtb14

### Some only have data for sub-regions. 
w<-which(w_data$all_areas_covered_new < 1)
length(w) # 226
length(unique(w_data$iso3))

### Remove those with only 1 MDR datapoint - not any more
n_data <- w_data %>% group_by(iso3) %>% count("iso3")
n_data1 <- w_data[-w,] %>% group_by(iso3) %>% count("iso3")
s <- setdiff(n_data$iso3, n_data1$iso3) # 8 removed by this
length(s)
intersect(s, mdr30$iso3) # include 3 in the top MDR! 

wm <- match(final_list,n_data$iso3, nomatch = 0)
wm1 <- match(final_list,n_data1$iso3, nomatch = 0)
length(which(n_data[wm,"n"] < 2)) # 31 only have 1 datapoint - extra 5 from removing those with only sub-national
length(which(n_data1[wm1,"n"] < 2)) # 36 only have 1 datapoint - extra 5 from removing those with only sub-national
setdiff( n_data1[which(n_data1[wm1,"n"] < 2),"iso3"],n_data[which(n_data[wm,"n"] < 2),"iso3"])

rem_1_dp_m <- n_data[which(n_data$n < 2),]
rem_1_dp <- pull(rem_1_dp_m, iso3)

# remove these from the final_list
final_list_107 <- setdiff(final_list, rem_1_dp)
length(final_list) # 138 # Keep 1 data point countries now

# SAVE
setwd("~/Dropbox/MDR/")
write.csv(final_list,"138_final_list_included_countries.csv")

# Global region of those removed
mm <- merge(rem_1_dp,WHOkey[,c('iso3','g_whoregion')],by='iso3',all.x=TRUE) # add global region to country output

####***************************************************************************************************************************************####
# Average MDR over multiple measures in same year - only happens if sub_group? 
w_data_subreg <- w_data %>% dplyr::group_by(iso3,year_new) %>% dplyr::summarise(av_mdr_new_pcnt = mean(mdr_new_pcnt), 
                                                                  mhi = mean(mhi),
                                                                  mlo = mean(mlo))

w_data_subreg$year <- w_data_subreg$year_new
w_data_subreg2 <- w_data_subreg[w_data_subreg$iso3 %in% final_list,]

# Convert from a % to a proportion
w_data_subreg2$new_mdr_prop <- w_data_subreg2$av_mdr_new_pcnt / 100
w_data_subreg2$mhi <- w_data_subreg2$mhi # already a proportion: see who_data_drs.R
w_data_subreg2$mlo <- w_data_subreg2$mlo # already a proportion: see who_data_drs.R
length(unique(w_data_subreg2$iso3)) # 138 now

write.csv(w_data_subreg2, "~/Dropbox/MRC SD Fellowship/RESEARCH/MDR/WHO_data/new_who_edited_sub.csv") ### NEW STANDARD DATA TO USE - only has final list of countries
write.csv(w_data_subreg2, "~/Dropbox/MDR/new_who_edited_sub.csv") 
####***************************************************************************************************************************************####

#### FINAL LIST
# MDR tb 2018 percentage
wm <- match(final_list, mtb18$iso3, nomatch = 0)
perc_wm <- sum(mtb18[wm,"perc_total"]) # 82.8% 
perc_wm
round(mtb18[wm,"perc_total"],2) #
max(round(mtb18[wm,"perc_total"],2)) # one contributes 26% = CHINA

# tb 2014 percentage
wm <- match(final_list, tb14$iso3, nomatch = 0)
perc_wm <- sum(tb14[wm,"perc_tb_14"]) # 93.4%
perc_wm

# contribution of those missing
wmm <- match(rem_1_dp, mtb18$iso3, nomatch = 0)
round(mtb18[wmm,"perc_total"],2) # one contributes 2.5% = Nigeria. 5.1% = Pakistan
sum(100*mtb18[wmm,"mdr_inc_num"]/ totalmtb18) # 14% removed as only one point
mtb18[wmm,c("country","mdr_inc_num","perc_total")]

# TB 2014 percentage
wm <- match(final_list, whotb2014$iso3)
tbsizewm <- sum(whotb2014[wm,"e_inc_num"])
perc_wm <- 100*tbsizewm / totaltb14 # 74%
perc_wm
perc_tb_14 <- 100*whotb2014$e_inc_num / totaltb14
tb14 <- as.data.frame(cbind(whotb2014$e_inc_num,perc_tb_14))
tb14$iso3 <- whotb2014$iso3
tb14$final_list <- 0
tb14[wm,"final_list"] <- 1
colnames(tb14) <- c("e_inc_num","perc_tb_14","iso3","in_final_list")
ggplot(tb14, aes(x=iso3, y = perc_tb_14, fill = factor(in_final_list))) + 
  geom_bar(stat="identity") +coord_flip() + aes(x=reorder(iso3,perc_tb_14),y=perc_tb_14) 

ggplot(subset(tb14, perc_tb_14 > 0.5), aes(x=iso3, y = perc_tb_14, fill = factor(in_final_list))) + 
  geom_bar(stat="identity") +coord_flip() + aes(x=reorder(iso3,perc_tb_14),y=perc_tb_14) + 
  ggtitle("Removed those < 0.5%") + scale_fill_discrete("In\nfinal\ncountry\nlist?", labels = c("No","Yes"))+
  geom_text(aes(label=round(perc_tb_14,2)), position=position_dodge(width=0.9), hjust=-0.1)
ggsave("Countries_by_2014_TB_burden.pdf")

# SAVE
setwd("~/Dropbox/MDR/")
write.csv(final_list,"107_final_list_included_countries.csv")

#### OLD from run_cohort_ltbi_mdr.R
## Which countries? 
# cni <- unique(mdr_cn_best$country) 
# length(cni) # 160
# # Removed from H&D analysis:
# too_small_pop <- c("WSM","ABW","ATG","BHS","BLZ","BRB","BRN","CUW","FSM","GRD","GUM","ISL","KIR",
#                    "LCA","MDV","MLT","NCL","PYF","STP","SYC","TON","VCT","VIR","VUT")
# tb_cnt_mismatch <- c("AIA", "AND", "ANT", "ASM","BMU","COK","CYM","DMA","GRL","KNA","MCO",
#                      "MHL","MNP","MSR","NIU","NRU","PLW","SMR","SXM","TCA","TKL","TUV","VGB","WLF")
# cni <- setdiff(cni, too_small_pop) # removes 12
# cni <- setdiff(cni, tb_cnt_mismatch) # removes 20 not in the DS-ARI database = 140
# # In H&D but not in rundata_ari
# cni <- intersect(unique(rundata$iso3),cni) # removes 2
# length(cni)
