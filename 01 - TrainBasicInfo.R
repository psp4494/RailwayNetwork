# Try and get the basic info for all trains in India from which further data can
# be scraped

# Set the folder
cur <- file.choose()
setwd(dirname(cur))

# Load required libraries
library(pacman)
p_load("XML")

# Open browser and get data
listpage <- htmlParse("http://www.prokerala.com/travel/indian-railway/trains/")
listpage <- readHTMLTable(listpage)
basicTrainInfo <- listpage[[1]]
for (i in 2:length(listpage)-1)
{
    basicTrainInfo <- rbind(basicTrainInfo,listpage[[i]])
}
basicTrainInfo <- basicTrainInfo[-1,c(2:5)]  
colnames(basicTrainInfo) <- c("TrainNo", "TrainName", "From", "To")

# Write csv 
write.csv(basicTrainInfo, file = "data/basicTrainInfo.csv", row.names = F)

# Destructors
rm(list = ls())