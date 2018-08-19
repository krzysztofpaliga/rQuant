init_rQuant <- function () {
  require(rlist)
  require(anytime)
  require(quantmod)
  require(tidyverse)
  require(foreach)
  require(doParallel)
  require(zoo)

  rQuant <- list()

  rQuant$bollingerBandsCSV <- "bollingerBands.csv"

  rQuant$bollingerBands <- list()

  rQuant$bollingerBands$calculate <- function (historicalData, windowSize, normDist = FALSE) {
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
      normDist2LB <- paste("normDist2LB",i,sep="_")
      normDist2HB <- paste("normDist2Hb",i,sep="_")
      normDist2Avg <- paste("normDist2Avg",i,sep="_")
      historicalData %>%
        arrange(coin, time) ->
        hsitoricalData
      historicalData %>%
        group_by(coin) %>%
        mutate(!!avgCN := rollmeanr(high, k=i*24, fill=NA),
               !!sdCN := rollapplyr(high, width=i*24, FUN=sd, fill=NA)) ->
        tempHistoricalData

      tempHistoricalData[[sd2upCN]] <- tempHistoricalData[[avgCN]] + 2*tempHistoricalData[[sdCN]]
      tempHistoricalData[[sd2downCN]] <- tempHistoricalData[[avgCN]] - 2*tempHistoricalData[[sdCN]]
      if (normDist) {
        tempHistoricalData[[normDist2LB]] <- (tempHistoricalData[[sd2downCN]] - tempHistoricalData$high) / tempHistoricalData$high
        tempHistoricalData[[normDist2HB]] <- (tempHistoricalData[[sd2upCN]] - tempHistoricalData$high) / tempHistoricalData$high
        tempHistoricalData[[normDist2Avg]] <- (tempHistoricalData[[avgCN]] - tempHistoricalData$high) / tempHistoricalData$high
        tempHistoricalData <- tempHistoricalData[, c(normDist2LB, normDist2HB, normDist2Avg, avgCN, sdCN, sd2upCN, sd2downCN)]
      } else {
        tempHistoricalData <- tempHistoricalData[, c(avgCN, sdCN, sd2upCN, sd2downCN)]
      }
      tempHistoricalData
    }
    stopCluster(cluster)
    historicalData$fiveUp <- 1.05*historicalData$high

    historicalData <- cbind(historicalData, historicalDataBB)

    historicalData <- historicalData[complete.cases(historicalData),]

    return (historicalData)
  }

  rQuant$bollingerBands$initDb <- function(odbcName="cryptonoi.se", dbName="cryptocompare_histoHour", windowSize=1:30, normDist = TRUE) {
    Connection <- DBI::dbConnect(odbc::odbc(), odbcName)
    data <- tbl(connection, dbName)
    data %>% distinct(coin) %>% collect() -> coinNames

    cores <- detectCores()
    cluster <- makeCluster(cores[1]-1)
    registerDoParallel(cluster)

    bollingerBands <- foreach(i=1:nrow(coinNames), .combine = rbind) %dopar% {
      require(tidyverse)
      require(quantmod)
      require(zoo)
      require(dplyr)
      require(dbplyr)
      require(rQuant)

      rQuant <- init_rQuant();
      localConnection <- DBI::dbConnect(odbc::odbc(), odbcName)

      localData <- tbl(localConnection, dbName)

      localData %>% filter(coin == coinNames[i,]$coin) %>% collect() -> coinHistory

      coinsBollingerBands <- rQuant$bollingerBands$calculate(historicalData = coinHistory, windowSize = windowSize, normDist = normDist)

      return (coinsBollingerBands)

    }
    stopCluster(cluster)

    DBI::dbWriteTable(connection, paste0(dbName, "_bollingerBands"), bollingerBands)
    return (bollingerBands)
  }

  rQuant$bollingerBands$csvSave <- function(bollingerBands) {
    write.csv(historicalData, rQuant$bollingerBandsCSV)
  }

  rQuant$bollingerBands$csvLoad <- function() {
    if (file.exists(rQuant$bollingerBandsCSV)) {
      cat("Loading the most recent file, resulting parameters could differ")
      historicalData <- read.csv(rQuant$bollingerBandsCSV, stringsAsFactors = FALSE)[,-1]
      return (historicalData)
    } else {
      cat("File does not exist")
    }
  }

  return (rQuant)
}
