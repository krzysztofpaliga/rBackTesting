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
# connection <- DBI::dbConnect(odbc::odbc(), "cryptonoise")
# data <- tbl(connection, "bittrex_btc_minutely")
# data %>% collect() -> bittrex

# bittrex %>% select(time) %>% distinct() %>% arrange(time) -> timestamps
# for (i in 1:nrow(timestamps)) {
#   print(anytime(timestamps[i,1]$time/1000))
#   print(bittrex %>% filter(time == timestamps[i,1]$time) %>% count())
# }
#
# bittrex %>% select(time) %>% max() -> oldest
# bittrex %>% filter(time == oldest) %>% group_by(coin) %>% count()
#
# while (TRUE) {
#   data %>% filter(coin == "ETH") %>% collect() -> a
#   print(a)
#   Sys.sleep(10)
# }


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
connection <- DBI::dbConnect(odbc::odbc(), "cryptonoi.se")
table <- tbl(connection, "cryptocompare_cryptopia_minutely")
table %>% filter(time > 1515511248) %>% collect() -> data

rQuant <- init_rQuant()

candleStickSampleWindowBuy <- 12
candleStickSampleWindowSell <- 12

avgSampleWindowBuy = candleStickSampleWindowBuy
avgSampleWindowSell = candleStickSampleWindowSell

data %>%
  group_by(coin) %>%
  mutate(avgRWBuy = rollmeanr(close, k = avgSampleWindowBuy, fill = NA)) -> data

data %>%
  group_by(coin) %>%
  mutate(avgRWSell = rollmeanr(close, k = avgSampleWindowSell, fill = NA)) -> data

na.omit(data) -> data

data$ascBuy <-
  rollapply(
    data$avgRWBuy,
    width = 3,
    FUN = asc,
    fill = NA,
    align = "right"
  )

data$ascSell <-
  rollapply(
    data$avgRWSell,
    width = 2,
    FUN = asc,
    fill = NA,
    align = "right"
  )

dataSampleWindowBuy <- data
dataSampleWindowBuy$open <-
  rollapply(
    data$open,
    width = candleStickSampleWindowBuy,
    FUN = open,
    fill = NA,
    align = "right"
  )

dataSampleWindowBuy$close <-
  rollapply(
    data$close,
    width = candleStickSampleWindowBuy,
    FUN = close,
    fill = NA,
    align = "right"
  )

dataSampleWindowBuy$high <-
  rollapply(
    data$high,
    width = candleStickSampleWindowBuy,
    FUN = high,
    fill = NA,
    align = "right"
  )

dataSampleWindowBuy$low <-
  rollapply(
    data$low,
    width = candleStickSampleWindowBuy,
    FUN = low,
    fill = NA,
    align = "right"
  )

dataSampleWindowSell <- data

dataSampleWindowSell$open <-
  rollapply(
    data$open,
    width = candleStickSampleWindowSell,
    FUN = open,
    fill = NA,
    align = "right"
  )

dataSampleWindowSell$close <-
  rollapply(
    data$close,
    width = candleStickSampleWindowSell,
    FUN = close,
    fill = NA,
    align = "right"
  )

dataSampleWindowSell$high <-
  rollapply(
    data$high,
    width = candleStickSampleWindowSell,
    FUN = high,
    fill = NA,
    align = "right"
  )

dataSampleWindowSell$low <-
  rollapply(
    data$low,
    width = candleStickSampleWindowSell,
    FUN = low,
    fill = NA,
    align = "right"
  )

dataSampleWindowBuy <-
  rQuant$candlesticks$calculate(dataSampleWindowBuy)
dataSampleWindowSell <-
  rQuant$candlesticks$calculate(dataSampleWindowSell)

na.omit(data) -> data
na.omit(dataSampleWindowBuy) -> dataSampleWindowBuy
na.omit(dataSampleWindowSell) -> dataSampleWindowSell

dataSampleWindowBuy$sampleWindow <- sampleWindowBuy
dataSampleWindowBuy$buy <- TRUE
dataSampleWindowSell$sampleWindow <- sampleWindowSell
dataSampleWindowSell$buy <- FALSE

windowSize = 12
tradeBookCostRatio = 0
binStrategy <- binStrategies$distributingMeanBinStrategy
buyStrategy <- buyStrategies$cc$hammerBuyStrategy
sellStrategy <- sellStrategies$cc$hammerSellStrategy
numberOfBins = 70
initialInvestment = 1

bins <- winRollingWindow(
  data = data,
  dataBuy = dataSampleWindowBuy,
  dataSell = dataSampleWindowSell,
  windowSize = windowSize,
  binStrategy = binStrategy,
  buyStrategy = buyStrategy,
  sellStrategy = sellStrategy,
  tradeBookCostRatio = tradeBookCostRatio,
  numberOfBins = numberOfBins,
  initialInvestment = initialInvestment
)
