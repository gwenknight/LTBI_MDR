### Generate the trends in MDR to use

library(ggplot2)

#### Linear curves to use
ari_store[82:162,"mdr"] <-c(rep(0,1970-1934),seq(0,mdr_last0,length=(2014-1969))) * rr$Av


#### Sigmoid curves to use
sigm <- function(x,delta,max,B=1){ max/(1+exp(-B*(x-delta)))} 

# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
jump_point <- c(41,46,51,56,61,66,71,76)
# tilt-how much of a gradient?
tilt <-seq(0.01,1,0.05)

save_jps <- c()
index = 1
for(i in 1:length(jump_point)){
  plot(  seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02))
  
  for(j in 1:length(tilt)){
    points(seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j]),type="l")
    abline(v = 1970,lty="dashed")
    if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[37] < 0.0002){
      if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[81] > (0.02-0.002)){
        save_jps <- rbind(save_jps,
                          cbind(index, i,j,seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])))
        index = index + 1
      }
    }
    
  }
}
save_jps <- as.data.frame(save_jps)
colnames(save_jps) <-c("index","jump","tilt","year","out")

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() + facet_wrap(~jump) + geom_vline(xintercept = 1970)

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() +  geom_vline(xintercept = 1970)


#### Quadratic curves to use
quad <- function(x,maxv){ 
  ll <-length(x)
  if(ll < 81){
    ff <- maxv/(ll^2)*(x^2)
    ff2 <- c(matrix(0,81-ll,1),ff)
  }else{ff2 <- maxv/(ll^2)*(x^2)}
  # 
  # 
  # w<-which( x < when)
  # if(length(w)>0){
  #   print("here")
  #   ff <- matrix(0,length(w),1)
  #   ff2 <-c(ff,max/((80-when)^2)*(x[w]^2))
  # } else{ ff2 <-max/((80-when)^2)*(x^2) }
  return(ff2)
} 


plot( seq(1934,2014,1),quad(seq(0,50,1),0.02))

# when increase after 1934? 1975, 1980, 1985, 1990, 1995, 2000
lengthx = seq(5,40,1)

save_quads <- c()
index = 1
for(i in 1:length(lengthx)){
  plot(  seq(1934,2014,1),quad(seq(0,lengthx[i],1),0.02))
  
  if(quad(seq(0,lengthx[i],1),0.02)[37] < 0.0002){
    if(quad(seq(0,lengthx[i],1),0.02)[81] > (0.02-0.002)){
      save_quads <- rbind(save_quads,
                          cbind(index,i,seq(1934,2014,1),quad(seq(0,lengthx[i],1),0.02)))
      index = index + 1
    }
  }
  
}

save_quads <- as.data.frame(save_quads)
colnames(save_quads) <-c("index","i","year","out")

ggplot(save_quads, aes(x=year,y=out,group=index, colour=index)) +  
  geom_line() +  geom_vline(xintercept = 1970)

## Together
ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() +  geom_vline(xintercept = 1970) + 
  geom_line(data = save_quads,aes(x=year,y=out,group=index, colour=factor(index)))

## Save
save_quads$tilt = 0
save_quads$jump = 0
save_jps$i = 0
save_jps$type = 1
save_quads$type = 2

save_curves <- rbind(save_jps, save_quads)
setwd("~/Dropbox/MRC SD Fellowship/Research/MDR/Latent MDR/Data/")
write.csv(save_curves,"sig_quad_curves.csv")
