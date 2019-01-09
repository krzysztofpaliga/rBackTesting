source("~/Develop/R/rTrade/R/BinStrategies.R")
source("~/Develop/R/rTrade/R/BuyStrategies.R")
source("~/Develop/R/rTrade/R/SellStrategies.R")
source("~/Develop/R/rTrade/R/Win.R")
require(rChange)
require(rQuant)
require(tidyverse)
require(dplyr)
require(odbc)
require(lubridate)
'%ni%' <- Negate('%in%')

asc <- function(a) {
  allAsc <- TRUE
  for (i in 1:(length(a) - 1)) {
    if (a[i] > a[i + 1]) {
      allAsc <- FALSE
    }
  }
  return(allAsc)
}

open <- function(a) {
  return(a[1])
}

close <- function(a) {
  return(a[length(a)])
}

low <- function(a) {
  lowest <- 0
  for (i in 1:length(a)) {
    if (a[i] < lowest) {
      lowest <- a[i]
    }
  }
  return(lowest)
}

high <- function(a) {
  highest <- 0
  for (i in 1:length(a)) {
    if (a[i] > highest) {
      highest <- a[i]
    }
  }
  return(highest)
}

for (i in 1:4320) {
  connection <- DBI::dbConnect(odbc::odbc(), "cryptonoi.se")
  table <- tbl(connection, "cryptocompare_cryptopia_minutely")
  table %>% filter(time > 1515511248) %>% collect() -> data
  DBI::dbDisconnect(connection)

  rQuant <- init_rQuant()

  candleStickSampleWindow <- i
  avgSampleWindow = candleStickSampleWindow
  data %>%
    group_by(coin) %>%
    mutate(avgRW = rollmeanr(close, k = avgSampleWindow, fill = NA)) -> data

  na.omit(data) -> data

  ataSampleWindow <- data
  dataSampleWindow$open <-
    rollapply(
      data$open,
      width = candleStickSampleWindow,
      FUN = open,
      fill = NA,
      align = "right"
    )

  dataSampleWindow$close <-
    rollapply(
      data$close,
      width = candleStickSampleWindow,
      FUN = close,
      fill = NA,
      align = "right"
    )

  dataSampleWindow$high <-
    rollapply(
      data$high,
      width = candleStickSampleWindow,
      FUN = high,
      fill = NA,
      align = "right"
    )

  dataSampleWindow$low <-
    rollapply(
      data$low,
      width = candleStickSampleWindow,
      FUN = low,
      fill = NA,
      align = "right"
    )

  dataSampleWindow <-
    rQuant$candlesticks$calculate(dataSampleWindow)

  na.omit(dataSampleWindow) -> dataSampleWindow
  dataSampleWindow$sampleWindow <- candleStickSampleWindow

  connection2 <- DBI::dbConnect(odbc::odbc(), "cryptonoi.se")
  DBI::dbWriteTable(connection2,
                    "cryptopia_sampleWindow",
                    dataSampleWindow,
                    append = TRUE)
  DBI::dbDisconnect(connection2)

  rm(data)
  rm(dataSampleWindow)
}
