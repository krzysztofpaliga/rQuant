context("rQuant")
setwd("../../")
source("R/RQuant.R")

require(rChange)

kucoinAPI <- initKucoinAPI()
kucoin <- initKucoin(kucoinAPI = kucoinAPI)
rQuant <- initRQuant()
data <- kucoin$getAllCoinsHistorical(addNewest = FALSE)

test_that("can initialize rQuant properly", {
  expect_equal(class(rQuant), "list")
})

test_that("rQuant$bollingerBands enriches historical data by $fiveUp", {
  historicalData <- rQuant$bollingerBands(historicalData=data, windowSize = 5)
  expect_equal(class(historicalData$fiveUp), "numeric")
})
