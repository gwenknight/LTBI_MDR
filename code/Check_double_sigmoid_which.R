### Which countries had double sigmoid as best fit curve?

mdr_cn_best <- read.csv("~/Documents/LTBI_MDR/output/store_cn_best.csv", stringsAsFactors = FALSE)[-1]
setwd("/Users/macbook_IC/Dropbox/MDR/output")
s_level <- read.csv("s_level_7.csv")[,-1]

# Which countries are in s_level? 
cn <- unique(s_level$pop_name)

# Which countries had double sigmoid?
