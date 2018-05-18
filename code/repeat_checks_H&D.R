#### Check repeats of H&D

setwd("~/Documents/LTBI_MDR/")
s_size <- c(200,1000)
nruns = 20
MID_all <- as.data.frame(matrix(0,nruns*7,7))

for(ik in 1:2){
  sample_size = s_size[ik]
  
  for(iij in 1:nruns){
    print(iij)
    source("code/GPreg.R")
    source("code/postARIanalysis.R")
    MID$run <- iij
    MID_all[((iij-1)*7 + 1):(iij*7),] <- MID
  }
  
  MIDa <- as.data.frame(MID_all)
  colnames(MIDa) <- colnames(MID)
  
  paper <- as.data.frame(cbind(c("AFR","AMR","SEA","EMR","WPR","EUR","GLOBAL"),c(22.4,11.0,30.8,16.3,27.9,13.7,23.0)/100),stringsAsFactors=FALSE) # from paper
  colnames(paper) <- c("g_whoregion", "paper")
  
  g_who_label = c("AFR","AMR","EMR","EUR","SEA","WPR","GLOBAL")
  MIDa$g_whoregion <- g_who_label[MIDa$g_whoregion]
  
  mMIDa <- merge(MIDa,paper, by = "g_whoregion")
  
  mMIDa$pLTBI<-100*(mMIDa$LTBI - as.numeric(mMIDa$paper)) / as.numeric(mMIDa$paper)
  
  #mMIDa$LTBI <- 100*(mMIDa$LTBI - as.numeric(mMIDa$paper))/as.numeric(mMIDa$paper)
  
  setwd("output")
  ggplot(mMIDa,aes(x=run,y=pLTBI)) + geom_point() + facet_wrap(~g_whoregion) + 
    scale_y_continuous("Percentage change from paper",lim=c(-5,5)) +
    ggtitle(paste0(sample_size," runs")) + scale_x_continuous("Replicate") + geom_hline(yintercept = 0)
  ggsave(paste0("mida1_",sample_size,".pdf"))
  write.csv(mMIDa,paste0("mida1_",sample_size,".csv"))
  
}

#### Averages
m200 <- read.csv("mida1_200.csv")[,-1]
m1000 <- read.csv("mida1_1000.csv")[,-1]
m200$repl <- 200
m1000$repl <- 1000

r_all <- rbind(m200,m1000)
summr <- r_all %>% group_by(repl,g_whoregion,paper) %>% summarise(ml = mean(x = LTBI), sd = sd(LTBI))

ggplot(summr, aes(x=repl, y = ml )) + geom_errorbar(aes(ymin = ml - sd, ymax = ml + sd)) + facet_wrap(~g_whoregion) + 
  geom_line(aes(y=paper),col="red") + scale_y_continuous("LTBI") + scale_x_continuous("Number of replicates")
ggsave("sample_size_mean.pdf")

