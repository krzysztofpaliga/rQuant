require(odbc)
require(dplyr)
require(dbplyr)
con <- DBI::dbConnect(odbc::odbc(), "cryptonoi.se")
cryptocompare_histoHour <- tbl(con, "cryptocompare_histoHour")
cryptocompare_histoHour %>%
  filter(coin == 'ETH' || coin == 'LTC') ->
  etc_ltc
etc_ltc %>% show_query()
etc_ltc %>% collect() -> df
