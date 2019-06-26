source("~/Develop/R/rBackTesting/R/BinStrategies.R")
source("~/Develop/R/rBackTesting/R/BuyStrategies.R")
source("~/Develop/R/rBackTesting/R/SellStrategies.R")
source("~/Develop/R/rBackTesting/R/Win.R")
require(rData)
#require(rChange)
require(rQuant)
require(tidyverse)
require(dplyr)
require(plyr)
require(odbc)
require(lubridate)
require(zoo)
rData <- init_rData()
data <- rData$cryptoCompare$getAllCoinsHisto(histoFunction = rData$cryptoCompare$API$histoDay, exchange="Binance", currency="BTC")
'%ni%' <- Negate('%in%')
#connection <- DBI::dbConnect(odbc::odbc(), "cryptocompare")
#datA <- tbl(connection, "cryptocompare_histoDay")
#datA <- tbl(connection, "binance_day")
#datA %>% filter(time>1550644132) %>%  collect() -> dataHistorical
#datA %>%  filter(exchange == "Cryptopia", currency == "BTC") %>% distinct() %>% collect() -> dataHistorical
#datA %>%  filter(exchange == "Binance", currency == "BTC") %>% distinct() %>% collect() -> dataHistorical
#datA %>%  filter(exchange == "Cryptopia", currency=="BTC") %>% distinct() %>% collect() -> dataHistorical

#datA %>% filter(time > 0) %>% collect()-> dataHistoricalF

rQuant <- init_rQuant()

#data <- rQuant$bollingerBands$calculate(dataHistorical, 12, 1, TRUE)
dat <- rQuant$candlesticks$calculate(dataHistorical)

#data <- na.omit(data)
data <- dat %>% filter(time > 1553529971)

asc <- function(a) {
  allAsc <- TRUE
  for (i in 1:(length(a)-1)) {
    if(a[i] > a[i+1]) {
      allAsc <- FALSE
    }
  }
  return(allAsc)
}

data$ascSell <- rollapply(data$close, width=2, FUN = asc, fill = NA, align = "right")
data$ascBuy <- rollapply(data$close, width=3, FUN = asc, fill = NA, align = "right" )

data <- na.omit(data)

windowSize = 12
tradeBookCostRatio = 0
binStrategy <- binStrategies$distributingMeanBinStrategy
buyStrategy <- buyStrategies$cc$hammerBuyStrategy
sellStrategy <- sellStrategies$cc$hammerSellStrategy
numberOfBins = 34
initialInvestment = 1

bins <- win(data = data,
    windowSize = windowSize,
    binStrategy = binStrategy,
    buyStrategy = buyStrategy,
    sellStrategy = sellStrategy,
    tradeBookCostRatio = tradeBookCostRatio,
    numberOfBins = numberOfBins,
    initialInvestment = initialInvestment)

