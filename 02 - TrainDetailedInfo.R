# Scrape more detailed data from the available basic data

# Set the folder
cur <- file.choose()
setwd(dirname(cur))

# Load required libraries
library(pacman)
p_load("XML")
p_load("scrapeR")
p_load("rvest")
p_load("RSelenium")
p_load("R2HTML")
p_load("lubridate")

# Load and start server
checkForServer()
startServer()

# Read available files
listTrains <- read.csv("data/basicTrainInfo.csv", stringsAsFactors = F)

# Create dataframe to be filled
n <- 6000
detailedTrainData <- data.frame(trainNo = character(n), trainName = character(n), startLocation = character(n),
                 stopLocation = character(n), noOfStops = integer(n), links = character(n),
                 linkDistances = character(n), startTime = character(n),
                 stopTime = character(n), totalDistance = numeric(n), totalTime = numeric(n),
                 avgSpeed = numeric(n), type = character(n), zone = character(n),
                 stringsAsFactors=FALSE) 

# Scrape data
for(i in 1:nrow(listTrains))
{
    # Get data for a single train
    remDr <- remoteDriver(browserName="firefox", port=4444)
    page <- paste("http://etrain.info/in#!TRAIN=", listTrains$TrainNo[i], sep = "")
    remDr$open()
    remDr$navigate(page)
    elem <- NULL
    try(elem <- remDr$findElement(using="id", value="schtbl"), silent = T)
    if(!is.null(elem))
    {
        elemtxt <- elem$getElementAttribute("outerHTML")
        elem2 <- remDr$findElement(using="css selector", value=".dborder~ .dborder+ .dborder .nobl")
        trainType <- elem2$getElementAttribute("outerHTML")
        elem3 <- remDr$findElement(using="css selector", value=".nobl+ td")
        trainZone <- elem3$getElementAttribute("outerHTML")
        HTMLSetFile("temp.html")
        HTML(elemtxt)
        trainData <- readHTMLTable("temp.html")
        print(paste("Data obtained for",listTrains$TrainNo[i],"at position",i))
        trainData <- as.data.frame(trainData)
        trainData <- trainData[-2,-1]
        colnames(trainData) <- c("code", "station", "arrival", "departure", "km")
        trainData <- trainData[complete.cases(trainData),]
        
        # Fill data extracted into frame
        detailedTrainData$trainNo[i] <- listTrains$TrainNo[i]
        detailedTrainData$trainName[i] <- listTrains$TrainName[i]
        detailedTrainData$startLocation[i] <- listTrains$From[i]
        detailedTrainData$stopLocation[i] <- listTrains$To[i]
        tempLoc <- trainData$code
        detailedTrainData$noOfStops[i] <- length(tempLoc)
        detailedTrainData$links[i] <- toString(tempLoc)
        tempLoc <- as.integer(as.character(trainData$km))
        detailedTrainData$linkDistances[i] <- toString(diff(tempLoc))
        detailedTrainData$startTime[i] <- as.character(trainData$departure[1])
        detailedTrainData$stopTime[i] <- as.character(trainData$arrival[nrow(trainData)])
        detailedTrainData$totalDistance[i] <- as.numeric(as.character(trainData$km[nrow(trainData)]))
        start <- as.character(trainData$departure[1])
        start <- strsplit(start,split = " ")[[1]][1]
        start <- as.POSIXlt(strptime(start, "%H:%M"))
        stop <- as.character(trainData$arrival[nrow(trainData)])
        stop <- strsplit(stop,split = " ")[[1]]
        stop <- as.POSIXlt(strptime(stop[1], "%H:%M")) + 86400 * (as.numeric(gsub(")", "", stop[3])) - 1)
        detailedTrainData$totalTime[i] <- difftime(stop, start, units = "hours")
        detailedTrainData$avgSpeed[i] <- detailedTrainData$totalDistance[i] / detailedTrainData$totalTime[i]
        trainType <- strsplit(as.character(trainType), split = ": ")[[1]][2]
        trainType <- gsub("</td>", "", trainType)
        detailedTrainData$type[i] <- trainType
        trainZone <- strsplit(as.character(trainZone), split = ": ")[[1]][2]
        trainZone <- gsub("</td>", "", trainZone)
        detailedTrainData$zone[i] <- trainZone
        rm(trainData, elem, elem2, elem3, elemtxt, page, start, stop, tempLoc, trainType, trainZone)
        file.remove("temp.html")
        print(paste("Entered for",listTrains$TrainNo[i],"at position",i))
    }
    remDr$close()
}

detailedTrainData <- detailedTrainData[complete.cases(detailedTrainData),]

# Find missing trains
missingTrains <- setdiff(listTrains$TrainNo, detailedTrainData$trainNo)

# Find data for missing trains
######
#To Be DONE


######
# Write csv 
write.csv(detailedTrainData, file = "data/detailedTrainData.csv", row.names = F)

# Destructors
rm(list = ls())