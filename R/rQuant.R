init_rQuant <- function () {
  require(rlist)
  require(anytime)
  require(quantmod)
  require(tidyverse)
  require(foreach)
  require(doParallel)
  require(zoo)
  require(odbc)

  rQuant <- list()

  rQuant$bollingerBandsCSV <- "bollingerBands.csv"

  rQuant$bollingerBands <- list()

  rQuant$bollingerBands$calculate <- function (historicalData, windowSize, samplesInWindow, normDist = FALSE) {
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
        mutate(!!avgCN := rollmeanr(close, k=i * samplesInWindow, fill=NA),
               !!sdCN := rollapplyr(close, width=i * samplesInWindow, FUN=sd, fill=NA)) ->
        tempHistoricalData

      tempHistoricalData[[sd2upCN]] <- tempHistoricalData[[avgCN]] + 2*tempHistoricalData[[sdCN]]
      tempHistoricalData[[sd2downCN]] <- tempHistoricalData[[avgCN]] - 2*tempHistoricalData[[sdCN]]
      if (normDist) {
        tempHistoricalData[[normDist2LB]] <- (tempHistoricalData$close - tempHistoricalData[[sd2downCN]]) / tempHistoricalData$close
        tempHistoricalData[[normDist2HB]] <- (tempHistoricalData[[sd2upCN]] - tempHistoricalData$close) / tempHistoricalData$close
        tempHistoricalData[[normDist2Avg]] <- (tempHistoricalData$close - tempHistoricalData[[avgCN]]) / tempHistoricalData$close
        tempHistoricalData <- tempHistoricalData[, c(normDist2LB, normDist2HB, normDist2Avg, avgCN, sdCN, sd2upCN, sd2downCN)]
      } else {
        tempHistoricalData <- tempHistoricalData[, c(avgCN, sdCN, sd2upCN, sd2downCN)]
      }
      tempHistoricalData
    }
    stopCluster(cluster)
    historicalData$fiveUp <- 1.05*historicalData$close

    historicalData <- cbind(historicalData, historicalDataBB)

    historicalData <- historicalData[complete.cases(select(historicalData, time, close, high, low, open, volumefrom, volumeto, exchange, coin, currency)),]

    return (historicalData)
  }

  rQuant$bollingerBands$initDb <- function(odbcName="cryptonoi.se", dbName="cryptocompare_histoDay", samplesInWindow=1, windowSize=2:30, normDist = TRUE) {
    connection <- DBI::dbConnect(odbc::odbc(), odbcName)
    print("Dropping db")
    DBI::dbSendQuery(connection, paste0("DROP TABLE IF EXISTS ", dbName, "_bollingerBands"))

    data <- tbl(connection, dbName)
    data %>% distinct(coin) %>% collect() -> coinNames

    for (i in 1:nrow(coinNames)) {
      print(paste0("Processing coin: ", coinNames[i,]$coin))
      data %>% filter(coin == coinNames[i,]$coin) %>% arrange(time) %>% collect() -> coinHistory

      coinsBollingerBands <- rQuant$bollingerBands$calculate(historicalData = coinHistory, samplesInWindow = samplesInWindow, windowSize = windowSize, normDist = normDist)

      print("Writing partial results to db")
      DBI::dbWriteTable(connection, paste0(dbName, "_bollingerBands"), coinsBollingerBands, append=TRUE)
    }
  }

  rQuant$bollingerBands$refreshDb <- function(odbcName="cryptonoi.se", dbName="cryptocompare_histoDay", samplesInWindow=1, windowSize=2:30, normDist = TRUE) {
    connection <- DBI::dbConnect(odbc::odbc(), odbcName)

    bb <- tbl(connection, paste0(dbName, "_bollingerBands"))
    #TODO? a bit hacky, but really simple and should work
    bb %>% filter(coin == 'ETH') %>% arrange(desc(time)) %>% head(1) %>% collect() -> eth_newest_row
    newestTime <- eth_newest_row$time
    if (as.numeric(Sys.time()) - newestTime < 24*60*60) {
      print("No need to refresh the database")
      return()
    }
    newestTimeWindowRange <- newestTime - max(windowSize)*24*60*60

    data <- tbl(connection, dbName)
    data %>% distinct(coin) %>% collect() -> coinNames
    for (i in 1:nrow(coinNames)) {
      print(paste0("Fetching coin data: ", coinNames[i,]$coin))
      data %>% filter(coin == coinNames[i,]$coin, time > newestTimeWindowRange) %>% arrange(time) %>% collect() -> coinHistory

      print(paste0("Processing coin data: ", coinNames[i,]$coin))
      coinsBollingerBands <- rQuant$bollingerBands$calculate(historicalData = coinHistory, samplesInWindow = samplesInWindow, windowSize = windowSize, normDist = normDist)
      coinsBollingerBands %>% filter(time > newestTime) -> coinsBollingerBands

      if (nrow(coinsBollingerBands) > 0) {
        print("Writing partial results to db")
        DBI::dbWriteTable(connection, paste0(dbName, "_bollingerBands"), coinsBollingerBands, append = TRUE)
      } else {
        print("No new results")
      }
    }
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

  # Candle Sticks
  rQuant$candlesticks <- list()
  rQuant$candlesticks$calculate <- function(historicalData) {
    historicalData %>%
    group_by(coin) %>%
      mutate(csBody = close - open,
             csNormBody = csBody / close,
             csUShadow = high - close,
             csNormUShadow = csUShadow / close,
             csLShadow = max(open, close) - low,
             csNormLShadow = csLShadow / close) ->
      historicalData

    return(historicalData)
  }
  return (rQuant)
}
