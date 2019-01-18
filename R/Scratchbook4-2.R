rTrade <- list()

rTrade$getAction <- function () {
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
  connection <- DBI::dbConnect(odbc::odbc(), "cryptonoise")
  datA <- tbl(connection, "bittrex_btc_minutely")

  fetchDataFrom <- (as.numeric(Sys.time()) - 50 * 60) * 1000

  datA %>%  filter(time >= fetchDataFrom) %>% collect() -> dataHistorical
  #datA %>%  collect() -> dataHistorical

  rQuant <- init_rQuant()

  dataHistorical$time <- dataHistorical$time / 1000

  dataHistorical$floor <- floor((dataHistorical$time %/% (5 * 60)) * (5 * 60))

  dataHistorical <- dataHistorical %>%
    group_by(coin, floor) %>%
    summarize(time = first(time),
              open = first(open),
              close = last(close),
              low = min(low),
              high = max(high))

  data <- rQuant$candlesticks$calculate(dataHistorical)

  fd <- function(a) {
    return((a[2] - a[1]))
  }

  asc <- function(a) {
    a<- na.omit(a)
    sum <- sum(a)
    if (sum > 0) {
      return (TRUE)
    } else {
      return (FALSE)
    }
  }
  desc <- function(a) {
    a<- na.omit(a)
    sum <- sum(a)
    if (sum > 0) {
      return (TRUE)
    } else {
      return (FALSE)
    }
  }
  mean <- mean(data$open)
  data$fd <- rollapply(data$close, width=2, FUN = fd, fill = NA, align = "right")
  data$ascSell <- rollapply(data$fd, width=5, FUN = asc, fill = NA, align = "right")
  data$ascBuy <- rollapply(data$fd, width=10, FUN = desc, fill = NA, align = "right" )
  data %>% mutate(green <- close > open) -> data
  #data <- na.omit(data)

  windowSize = 12
  tradeBookCostRatio = 0
  binStrategy <- binStrategies$distributingMeanBinStrategy
  buyStrategy <- buyStrategies$cc$hammerBuyStrategy
  sellStrategy <- sellStrategies$cc$hammerSellStrategy
  numberOfBins = 50
  initialInvestment = 0.1

  lastData <- data %>% filter(time > max(data$time))

  bins <- win(data = lastData,
              windowSize = windowSize,
              binStrategy = binStrategy,
              buyStrategy = buyStrategy,
              sellStrategy = sellStrategy,
              tradeBookCostRatio = tradeBookCostRatio,
              numberOfBins = numberOfBins,
              initialInvestment = initialInvestment)
  return(bins)
}


