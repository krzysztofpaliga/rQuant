initRQuant <- function () {
  require(rlist)
  require(anytime)
  require(quantmod)
  require(tidyverse)
  require(foreach)
  require(doParallel)

  rQuant <- list()

  rQuant$bollingerBandsCSV <- "bollingerBands.csv"

  rQuant$bollingerBands <- function (historicalData, windowSize, load=FALSE, save=FALSE) {
    if (load) {
      if (file.exists(rQuant$bollingerBandsCSV)) {
        cat("Loading the most recent file, resulting parameters could differ")
        historicalData <- read.csv(rQuant$bollingerBandsCSV, stringsAsFactors = FALSE)[,-1]
        return (historicalData)
      } else {
        cat("File does not exist")
      }
    }
    cores <- detectCores()
    cluster <- makeCluster(cores[1]-1)
    registerDoParallel(cluster)

    historicalDataBB <- foreach(i=windowSize, .combine=cbind) %dopar% {
      require(tidyverse)
      require(quantmod)
      require(zoo)
      avgCN <- paste("avg",i,sep="_")
      sdCN<- paste("sd",i,sep="_")
      sd2upCN <- paste("sd2up",i,sep="_")
      sd2downCN <- paste("sd2down",i,sep="_")
      historicalData %>%
        arrange(cc, date) %>%
        mutate(!!avgCN := rollmeanr(high, k=i*24, fill=NA),
               !!sdCN := rollapplyr(high, width=i*24, FUN=sd, fill=NA)) ->
        tempHistoricalData

      tempHistoricalData[[sd2upCN]] <- tempHistoricalData[[avgCN]] + 2*tempHistoricalData[[sdCN]]
      tempHistoricalData[[sd2downCN]] <- tempHistoricalData[[avgCN]] - 2*tempHistoricalData[[sdCN]]

      tempHistoricalData <- tempHistoricalData[, c(avgCN, sdCN, sd2upCN, sd2downCN)]
      tempHistoricalData
    }
    stopCluster(cluster)
    historicalData$fiveUp <- 1.05*historicalData$high

    historicalData <- cbind(historicalData, historicalDataBB)

    historicalData %>%
      na.omit() ->
      historicalData

    if (save) {
      write.csv(historicalData, rQuant$bollingerBandsCSV)
    }
    return (historicalData)
  }

  return (rQuant)
}
