#### Sigmoid curves to use

library(ggplot2)

home <- "~/Documents/LTBI_MDR/"
output <- "~/Documents/LTBI_MDR/output/"

## Function
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
    if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[37] < 0.0002){ # 1970 ~ 0
      if(sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])[81] > 0.02 - 0.0002){ # 2014 ~ 0.02
        save_jps <- rbind(save_jps,
                          cbind(index, i,j,seq(1934,2014,1),sigm(seq(0,80,1),jump_point[i],0.02,tilt[j])))
        index = index + 1
      }
    }
  }
}
save_jps <- as.data.frame(save_jps)
colnames(save_jps) <-c("index","jump","tilt","year","out")

setwd(output)
ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() + facet_wrap(~jump) + geom_vline(xintercept = 1970)

ggplot(save_jps, aes(x=year,y=out,group=index,colour = factor(tilt))) + 
  geom_line() +  geom_vline(xintercept = 1970)
ggsave("sigmoid_curves_include.pdf")

#### Quadratic curves to use
quad <- function(x,maxv){ 
  ll <-length(x)
  if(ll < 81){
    ff <- maxv/(ll^2)*(x^2)
    ff2 <- c(matrix(0,81-ll,1),ff)
  }else{ff2 <- maxv/(ll^2)*(x^2)}
  return(ff2)
} 


plot( seq(1934,2014,1),quad(seq(0,80,1),0.02))

# when increase after 1970? 1975, 1980, 1985, 1990, 1995, 2000
quad_start <- seq(45,15,-5)

save_qjps <- c()
index = 1
for(i in 1:length(quad_start)){
  plot(seq(1934,2014,1),quad(seq(0,quad_start[i],1),0.02))
      save_qjps <- rbind(save_qjps,
                        cbind(index, i,seq(1934,2014,1),quad(seq(0,quad_start[i],1),0.02)))
      index = index + 1
}
save_qjps <- as.data.frame(save_qjps)
colnames(save_qjps) <-c("index","start","year","out")

ggplot(save_qjps, aes(x=year,y=out,group=index,colour = factor(start))) + 
  geom_line() +  geom_vline(xintercept = 1970)
ggsave("quad_curves_include.pdf")


#### Linear curves to use
lin <- function(x,maxv){ 
  ll <-length(x)
  if(ll < 81){
    ff <- (maxv)/(ll)*x 
    ff2 <- c(matrix(0,81-ll,1),ff)
  }else{ff2 <- maxv/(ll)*x}
  return(ff2)
} 


plot( seq(1934,2014,1),lin(seq(0,45,1),0.02))

# when increase after 1970? 1975, 1980, 1985, 1990, 1995, 2000
lin_start <- seq(45,5,-5)

save_ljps <- c()
index = 1
for(i in 1:length(lin_start)){
  plot(seq(1934,2014,1),lin(seq(0,lin_start[i],1),0.02))
  save_ljps <- rbind(save_ljps,
                     cbind(index, i,seq(1934,2014,1),lin(seq(0,lin_start[i],1),0.02)))
  index = index + 1
}
save_ljps <- as.data.frame(save_ljps)
colnames(save_ljps) <-c("index","start","year","out")

ggplot(save_ljps, aes(x=year,y=out,group=index,colour = factor(start))) + 
  geom_line() +  geom_vline(xintercept = 1970)
ggsave("lin_curves_include.pdf")



##### ALSO ADD IN TRIANGULAR CDF: https://en.wikipedia.org/wiki/Triangular_distribution
