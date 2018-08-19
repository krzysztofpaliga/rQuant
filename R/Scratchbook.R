require(odbc)
require(dplyr)
require(dbplyr)
connection <- DBI::dbConnect(odbc::odbc(), "cryptonoi.se")
rQuant <- init_rQuant()
a <- rQuant$bollingerBands$initDb(connection, "cryptocompare_histoHour", 1:2, normDist = TRUE)
data <- tbl(connection, "cryptocompare_histoHour")
cryptocompare_histoHour %>%
  filter(coin == 'ETH' || coin == 'LTC') ->
  etc_ltc
etc_ltc %>% show_query()
etc_ltc %>% collect() -> df

rQuant <- init_rQuant()
bb <- rQuant$bollingerBands$calculate(historicalData = df, windowSize = 3:4, normDist = FALSE)

system.time({cryptocompare_histoHour %>% collect() -> df})
