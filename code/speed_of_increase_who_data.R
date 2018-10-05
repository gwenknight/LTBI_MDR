## Speed of increase - 5% Ok? 

# Country list and WHO data
who0 <- as.data.frame(read.csv("~/Dropbox/MDR/new_who_edited_sub.csv")[,-1])
who0$year <- who0$year_new
uu <- unique(who0$iso3) # 138 now
luu <- length(unique(who0$iso3)) # 138

grad <- c()
grad1 <- c()
grad2 <- c()

for(i in 1:luu){
  country <- uu[i]
  who_l <- who0 %>% filter(iso3==country) 
  mmdr <- tail(who_l$av_mdr_new_pcnt,1)/100
  year <- tail(who_l$year,1)
  grad <-c(grad,(mmdr - 0)/(year - 1970))
  grad1 <-c(grad1,(mmdr - 0)/(year - 1990))
  grad2 <-c(grad2,(mmdr - 0)/(year - 1980))
  
  
}

plot(grad)
mean(grad)
max(grad)


points(grad1, col = "red")
mean(grad1)
max(grad1)

points(grad2, col = "blue")
mean(grad2)
max(grad2)
sd(grad2)
quantile(grad2, prob = c(0.05,0.95)) #25-75: 0.0003 - 0.001


max(who0$av_mdr_new_pcnt)
