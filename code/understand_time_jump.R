m1 <- samples.p.p[1,]
t1 <- samples.p.t[1] #+ 1970
years1 <- years_to_predict #+ 1970

plot(years1, m1, type = "l")

plot(who_l$year, who_l$mdr_new, col = "red",type = "p", xlim = c(1970,2014), 
     ylim = c(0,max(who_l$mhi)+max(who_l$mhi)/2))
points(who_l$year, who_l$mlo, col = "red",pch=4)
points(who_l$year, who_l$mhi, col = "red",pch=4)
lines(years1, m1)
title(round(t1))


###
x = seq(-0.000001,2,0.01) 
plot(x,inv.logit(x)-0.5,type = "l")

plot(x-0.5,inv.logit(x-0.5),type = "l")

x = seq(-10,10,0.1)
plot(x, inv.logit(x),type = "l")

