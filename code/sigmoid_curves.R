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
      save_jps <- rbind(save_jps,
                        cbind(index, i,j,seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])))
      index = index + 1
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
quad <- function(x,when,maxv,B=1){ 
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


plot( seq(1934,2014,1),quad(seq(0,80,1),37,0.02))

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
      save_jps <- rbind(save_jps,
                        cbind(index, i,j,seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])))
      index = index + 1
    }
    
  }
}
save_jps <- as.data.frame(save_jps)
colnames(save_jps) <-c("index","jump","tilt","year","out")

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() + facet_wrap(~jump) + geom_vline(xintercept = 1970)

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() +  geom_vline(xintercept = 1970)
