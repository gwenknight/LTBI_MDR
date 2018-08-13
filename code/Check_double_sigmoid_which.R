### Which countries had double sigmoid as best fit curve?

mdr_cn_best <- read.csv("~/Documents/LTBI_MDR/output/store_cn_best.csv", stringsAsFactors = FALSE)[-1]
setwd("~/Dropbox/MDR/output")
s_level <- read.csv("s_level_7.csv", stringsAsFactors = FALSE)[,-1]
s_level$country <- s_level$pop_name

#dd <- merge(mdr_cn_best, s_level,by = "country")

# Which countries are in s_level? 
cn_list <- unique(s_level$pop_name) # 107

# Which countries had double sigmoid?
w<-intersect(which(s_level$rep == 4), which(s_level$best == 1))
length(w) # 51

ds_table <-s_level[w,c("pop_name","best","rep")]

n_data <- mdr_cn_best[order(mdr_cn_best$country)[!duplicated(sort(mdr_cn_best$country))],c("country","n_data")]
n_data$pop_name <- n_data$country

ds_table <- merge(ds_table, n_data[,c("pop_name","n_data")], by = "pop_name")

ds_table <- ds_table[order(ds_table$n_data,decreasing = TRUE),]

# decreasing trend possible? 
ds_table$trend_valid <- 0 # 0 if not. Only look at those with more than 2 points. 
ds_table_2 <- subset(ds_table, n_data > 2)  # 31, removes 20

# Copy files into one folder
for(i in 1:dim(ds_table_2)[1]){
  cn <- ds_table_2[i,1]
  current_folder <- "/Users/eideghkmi/Dropbox/MDR/output"
  new_folder <- "/Users/eideghkmi/Dropbox/MDR/output/DScurves/"
  file.copy(file.path(current_folder,paste0(cn,"_mdr_trends_4.pdf")), new_folder)
}

ds_table_2$reason <- ""
ds_table_2$other <- 0

ds_comment <- read.csv("ds_table_2.csv", stringsAsFactors = FALSE)
# those with a maybe decline: 
maybe_ds <- c("HKG","MAC","PRT","TJK","USA")


# Cohen 2014 decreasing trends
c14de <- c("AUS","USA","ISR","DEU","HKG","MAC","LVA","EST","PRT")
length(intersect(c14de, cn_list)) # 9 = all in the main analysis of countries
intersect(c14de, maybe_ds) # TJK not in Cohen list

for(i in 1:length(c14de)){
  cn <- c14de[i]
  current_folder <- "/Users/eideghkmi/Dropbox/MDR/output"
  new_folder <- "/Users/eideghkmi/Dropbox/MDR/output/DScurves_cohen/"
  file.copy(file.path(current_folder,paste0(cn,"_mdr_trends_4.pdf")), new_folder)
}
