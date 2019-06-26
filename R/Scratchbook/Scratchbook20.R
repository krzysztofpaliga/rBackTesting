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
require(prophet)
rData <- init_rData()
dat <- rData$cryptoCompare$getAllCoinsHisto(histoFunction = rData$cryptoCompare$API$histoDay, exchange="Binance", currency="BTC")
'%ni%' <- Negate('%in%')
#connection <- DBI::dbConnect(odbc::odbc(), "cryptocompare")
#datA <- tbl(connection, "cryptocompare_histoDay")
#datA <- tbl(connection, "binance_day")
#datA %>% filter(time>1550644132) %>%  collect() -> dataHistorical
#datA %>%  filter(exchange == "Cryptopia", currency == "BTC") %>% distinct() %>% collect() -> dataHistorical
#datA %>%  filter(exchange == "Binance", currency == "BTC") %>% distinct() %>% collect() -> dataHistorical
#datA %>%  filter(exchange == "Cryptopia", currency=="BTC") %>% distinct() %>% collect() -> dataHistorical

#datA %>% filter(time > 0) %>% collect()-> dataHistoricalF
data <- dat %>% filter(time > 1546358771)
data <- dat
ccs = data.frame(ds = anytime(dat$time), y= dat$close, coin = dat$coin)
ccs$time <- as.numeric(ccs$ds)

ccnamesd <- ccs %>% distinct(coin)
ccnames<- levels(droplevels(ccnamesd$coin))
#ccnames <- ccnames[order(ccnames$coin),]

objs <- new.env()

for (i in 1:length(ccnames[])) {
  ccname = ccnames[i]
  print(ccname)
  cc = ccs[ccs$coin==ccname,]
  cc = cc[c("y", "ds")]
  tryCatch({
    m <- prophet(cc[])
    future <- make_future_dataframe(m, periods=7)
    forecast <- predict(m, future)

    m$history$timestamp <- as.numeric(m$history$ds)
  }, error = function(error_condition) {})
  obj <- new.env()
  obj[['m']] = m
  obj[['future']] = future
  obj[['forecast']] = forecast
  objs[[ccname]] = obj
}

tradeBookCostRatio = 0
binStrategy <- binStrategies$distributingMeanBinStrategy
buyStrategy <- buyStrategies$prophet$buyUndervalued
sellStrategy <- sellStrategies$prophet$sellOvervalued
numberOfBins = 25
initialInvestment = 1

bins <- win(data = data,
            objs = objs,
    binStrategy = binStrategy,
    buyStrategy = buyStrategy,
    sellStrategy = sellStrategy,
    tradeBookCostRatio = tradeBookCostRatio,
    numberOfBins = numberOfBins,
    initialInvestment = initialInvestment)

