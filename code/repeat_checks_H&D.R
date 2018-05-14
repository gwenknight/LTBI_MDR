#### Check repeats of H&D

setwd("~/Documents/LTBI_MDR/")
nruns = 5
MID_all <- as.data.frame(matrix(0,nruns*7,7))

for(iij in 1:nruns){
  print(iij)
  source("code/postARIanalysis.R")
  MID$run <- iij
  MID_all[((iij-1)*7 + 1):(iij*7),] <- MID
}

MIDa <- as.data.frame(MID_all)
colnames(MIDa) <- colnames(MID)
MIDa$LTBI<-100*MIDa$LTBI
MIDa$LTBI <- MIDa$LTBI - c(22.4,11.0,30.8,16.3,27.9,13.7,23.0)
#ggplot(MID_all, aes(x=g_whoregion, y=LTBI)) + geom_bar(aes(fill = factor(run)), position = "dodge", stat="identity")

ggplot(MIDa,aes(x=run,y=LTBI)) + geom_point() + facet_wrap(~g_whoregion)
#write.csv(MIDa,"mida1.csv")
