context("rQuant")
setwd("../../")
source("R/rQuant.R")

require(rChange)

kucoinAPI <- initKucoinAPI()
kucoin <- initKucoin(kucoinAPI = kucoinAPI)
rQuant <- init_rQuant()
data <- kucoin$getAllCoinsHistorical(load=TRUE,addNewest = FALSE)

test_that("can initialize rQuant properly", {
  expect_equal(class(rQuant), "list")
})

test_that("rQuant$bollingerBands$calculate enriches historical data by $fiveUp", {
  historicalData <- rQuant$bollingerBands$calculate(historicalData=data, windowSize = 1:3, normDist = TRUE)
  expect_equal(class(historicalData$fiveUp), "numeric")
})
