#### Likelihood function
library(dplyr)

likelihood <- function(who_l, curve_l){
  # who_l = who data for a certain country
  # curve_l = predicted MDR ARI levels for certain years
  
  # DATA 
  n = dim(who_l)[1] # number of data points
  # From 95% CI
  who_l <- as.data.frame(who_l)
  who_l$sigma <- (who_l$mhi - who_l$mdr_new)/1.96 # assuming normal distribution
  who_l$sigma2 <- (who_l$sigma)^2
  # PREDICTED
  curves_use <- as.data.frame(filter(curve_l, year %in% who_l$year))
  if(length(curves_use$out) < length(who_l$mdr_new)){print("ERROR")}
  who_l$L <- (log(who_l$sigma) + 0.5 * log(2*pi)) + (who_l$mdr_new - curves_use$out)^2/(2*(who_l$sigma2))
  
  return(sum(who_l$L))
}



# #####******* Example use ******##########
# library(ggplot2)
# 
# ## Read in MDR curves
# save_curves0 <- read.csv("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent_MDR/Data/lin_sig_quad_sigd_curves.csv",stringsAsFactors = FALSE)[-1]
# curves <- subset(save_curves0, type == 1) # just linear
# 
# mdr_cn_best <- read.csv("~/Documents/LTBI_MDR/output/store_cn_best.csv", stringsAsFactors = FALSE)[-1]
# 
# ## WHO data
# setwd("~/Dropbox/MRC SD Fellowship/RESEARCH/MDR/WHO_data/")
# w_data <- read.csv("new_who_edited.csv",stringsAsFactors = FALSE)[,-1]
# w_data <- w_data[,c("country","iso3","year_new","mdr_new_pcnt","mlo","mhi")]
# who$mdr_new<- who$mdr_new_pcnt/100
# 
# ## likelihood
# curve <- subset(curves, index == 3)
# who <- subset(w_data, iso3 == "ALB")
# 
# # Scale curve
# curves$out <- tail(who,1)$mdr_new_pcnt * curves$out/0.02
# 
# 
# likelihood(who, curve) # generate likelihood of this curve 
# 
# ll <- c()
# for(i in 1:9){
#   w <- which(curves$index == i)
#   curve <- curves[w,]
#   ll <- c(ll,likelihood(who, curve))
# }
# plot(ll)
# who$index = 1; who$year = who$year_new
# who$out <- who$mdr_new_pcnt
# ggplot(curves, aes(x=year, y = out, group = index, colour = factor(index))) + geom_line() + 
#   geom_point(data = who)
# 
# 
# #### MOCK
# who_mock <- as.data.frame(cbind(c(2000,2010,2011),c(0,0.1,0.12),c(0.01,0.12,0.14)))
# colnames(who_mock) <- c("year","mdr_new","mhi")
# y = c(matrix(0,1,1970-1934),0.1 * seq(0,2014-1970,1))/100
# curves_mock_orig <- as.data.frame(cbind(seq(1934,2014,1),y)) # max = 0.044
# colnames(curves_mock_orig) <- c("year","out")
# likelihood(who_mock,curves_mock_orig)
# final_levels <- seq(0,0.2,length = 200)
# curves_mock <- curves_mock_orig
# 
# store_ll <- c()
# 
# for(j in 1:length(final_levels)){
#   mdr_last <- final_levels[j]
#   curves_mock$out <- mdr_last * curves_mock_orig$out/0.044 # rescale for this country 
#   
#   ggplot(curves_mock, aes(x=year, y = out, group = index)) + geom_line() + geom_point(data = who_mock, aes(x=year, y = mdr_new))
#   
#   store_ll <- rbind(store_ll,cbind(j, mdr_last, likelihood(who_mock,curves_mock)))
#   
# }
# 
# plot(store_ll[,2],store_ll[,3])
