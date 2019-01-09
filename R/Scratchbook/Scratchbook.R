require(rChange)
require(rQuant)
require(tidyverse)
require(dplyr)
require(odbc)
require(lubridate)
'%ni%' <- Negate('%in%')
connection <- DBI::dbConnect(odbc::odbc(), "cryptonoi.se")
datA <- tbl(connection, "cryptocompare_histoDay")
datA %>% filter(time>1538479017) %>%  collect() -> dataHistorical
rQuant <- init_rQuant()

data <- rQuant$candlesticks$calculate(dataHistorical)
data <- rQuant$bollingerBands$calculate(dataHistorical, 12, 1, TRUE)
#data <- na.omit(data)

asc <- function(a) {
  stopifnot(length(a) == 2)
  if(is.na(a[1]) || is.na(a[2])) {
    return (FALSE)
  }
  if (a[2]>a[1]) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

data$asc <- rollapply(data$avg_12, width=2, FUN = asc, fill = NA)
data$normDistMean <- (get("avg_12", data) - get("open", data)) / get("open",data)
data$normDistMeanAbs <- abs(data$normDistMean)

win <-
  function(data,
           initialInvestment,
           numberOfBins,
           windowSize,
           reinvestmentRatio,
           tradeBookCostRatio,
           returnThreshold,
           closeDistance) {
#     initialInvestment = 0.05
 #    numberOfBins = 20
#     windowSize = 9
#     reinvestmentRatio = 0.9
#     closeDistance = 0.001
    transactionHistory <-
      data_frame(
        date = as.POSIXct(character()),
        cc = character(),
        amount = double(),
        price = double(),
        value = double(),
        type = character()
      )

    reserveFund <- 0
    currentlyHeldReturn <- 0
    initialBinInvestment <- initialInvestment / numberOfBins
    bins <-
      data_frame(
        cc = character(),
        amount = double(),
        price = double(),
        reinvestmentBin = double(),
        binIndex = integer(),
        lastInvestmentHeight = double()
      )
    for (i in 1:numberOfBins) {
      bins %>%
        add_row(
          cc = NA,
          amount = NA,
          reinvestmentBin = initialBinInvestment,
          binIndex = i,
          lastInvestmentHeight = 0
        ) ->
        bins
    }

    multipliers <- distinct(data, coin)
    multipliers$factor <- 1

    normDist2LBCN <- paste("normDist2LB", windowSize, sep = "_")
    normDist2HBCN <- paste("normDist2Hb", windowSize, sep = "_")
    meanCN <- paste("avg", windowSize, sep="_")

    mks <- 0
    dk <- 0
    pointsInTime <- arrange(distinct(data, time), time)
    investmentHistory <- select(pointsInTime, time)
    investmentHistory$investment <- 0
    for (pointInTimeRowNumber in 1:nrow(pointsInTime)) {
      pointInTime <- pointsInTime[pointInTimeRowNumber, ]
      if (pointInTimeRowNumber > 1) {
        aPointInTimeDayBeforeRowNumber = pointInTimeRowNumber - 1
        aPointInTimeDayBefore <- data[aPointInTimeDayBeforeRowNumber, ]
        aPointInTime <- data[pointInTimeRowNumber,]
        trendAscending = get(meanCN,aPointInTime) - get(meanCN, aPointInTimeDayBefore) > 0
        if (aPointInTime$coin != aPointInTimeDayBefore$coin) {
          trendAscending = NA
        }
      } else {
        trendAscending = NA
      }

      if (day(anytime(pointInTime)) == 1) {
       # currentlyHeldReturn <- currentlyHeldReturn + 0.04
      }
      dataAtPointInTime <- filter(data, time == pointInTime)
      if (nrow(filter(bins, !is.na(cc)))> 0)  {
        toBeSold <-
          filter(dataAtPointInTime,
                 (coin %in% bins$cc) & get(normDist2HBCN) < 0)
                 #(coin %in% bins$cc))

        toBeSoldByTrend <-
          filter(dataAtPointInTime,
                  coin %in% bins$cc, get(normDist2HBCN) > 0, asc == FALSE, normDistMeanAbs <= closeDistance)
          if (nrow(toBeSoldByTrend) > 0) {
            toBeSold <- rbind(toBeSold, toBeSoldByTrend)
          }

        if (!is_empty(toBeSold) && nrow(toBeSold) > 0) {
          for (rowNumber in 1:nrow(toBeSold)) {
            row <- toBeSold[rowNumber, ]
            binIndexes <- filter(bins, cc == row$coin)$binIndex
            meanPrice <- mean(filter(bins, cc==row$coin)$price)
            for (binIndex in binIndexes) {
              if (row$high >=  returnThreshold * meanPrice) {
                multipliers[multipliers$coin == row$coin,]$factor <- 1
              heldAmount <- bins[binIndex, ]$amount
              if (heldAmount > 0) {
              return <- (1-tradeBookCostRatio)*0.998*row$high * heldAmount
              reserveFund <- reserveFund + (1-reinvestmentRatio) * return
              currentlyHeldReturn <- currentlyHeldReturn + reinvestmentRatio * return
              stopifnot(is.numeric(return))
              stopifnot(return > 0)
              stopifnot(is.numeric(currentlyHeldReturn))

              bins[binIndex,]$cc <- NA
              bins[binIndex,]$amount <- NA

              #print(paste("date", anytime(pointInTime), ":"))
              #print(paste("held return", currentlyHeldReturn, ":"))
              #print(paste("held reinvestment", sum(c(filter(bins, is.na(cc))$reinvestmentBin))))
              transactionHistory %>%
                add_row(
                  cc = row$coin,
                  date = anytime(row$time),
                  amount = heldAmount,
                  price = row$high,
                  value = return,
                  type = "sale"
                ) ->
                transactionHistory
              # print(paste("sold", heldAmount, "of", row$coin, "for", return, "at price of", row$high))
              # print(paste("refilled", binsToRefill, "bins"))
              # print(currentlyHeldCoins)
}
                }
            }
          }
          #(paste("reserve ", reserveFund))

          meanInvestmentHeight <- mean(bins$lastInvestmentHeight)
          currentlyInvestedFunds <- sum(filter(bins, !is.na(cc))$lastInvestmentHeight)
          currentlyHeldForReinvesting <- sum(bins$reinvestmentBin)
          binsToRefill <- floor((currentlyHeldForReinvesting + currentlyHeldReturn) / meanInvestmentHeight)
          emptyBins <- nrow(filter(bins, is.na(cc)))
          if (is.numeric(binsToRefill)) {
            if (binsToRefill == emptyBins) {
              # just enough funds to fill all empty bins, with some leftovers not enough to fill additional bin
              emptyBinsIndexes <- filter(bins, is.na(cc))$binIndex
              for (emptyBinIndex in emptyBinsIndexes) {
                currentlyHeldReturn <- currentlyHeldReturn + bins[emptyBinIndex,]$reinvestmentBin - meanInvestmentHeight
                bins[emptyBinIndex,]$reinvestmentBin <- meanInvestmentHeight
              }
            }
            if (binsToRefill > emptyBins) {
              # enough funds to fill more  bins, than there are empty ones, so redistribute to all bins
              toReinvestInEveryBin <- (currentlyHeldForReinvesting + currentlyHeldReturn) / numberOfBins
              stopifnot(is.numeric(toReinvestInEveryBin))
              for (binIndex in 1:numberOfBins) {
                currentlyHeldReturn <- currentlyHeldReturn + bins[binIndex,]$reinvestmentBin - toReinvestInEveryBin
                stopifnot(is.numeric(currentlyHeldReturn))
                bins[binIndex,]$reinvestmentBin <- toReinvestInEveryBin
              }
            }
            # if (binsToRefill < emptyBins) {
            #   # not enough funds to fill all empty bins, so there is not enough funds, to keep investition at the current level
            #   toReinvestInEveryBin <- (currentlyHeldForReinvesting + currentlyHeldReturn) / numberOfBins
            #   stopifnot(is.numeric(toReinvestInEveryBin))
            #   for (binIndex in 1:numberOfBins) {
            #     currentlyHeldReturn <- currentlyHeldReturn + bins[binIndex,]$reinvestmentBin - toReinvestInEveryBin
            #     stopifnot(is.numeric(currentlyHeldReturn))
            #     bins[binIndex,]$reinvestmentBin <- toReinvestInEveryBin
            #   }
            # }
          }
        }
      }
      if (nrow(filter(bins, is.na(cc))) > 0) {
        toBeBought <-
          # filter(dataAtPointInTime, get(normDist2LBCN) > 0 & (coin %ni% bins$cc))
          filter(dataAtPointInTime, get(normDist2LBCN) < 0)
        toBeBoughtByTrend <-
          filter(dataAtPointInTime,
                 get(normDist2LBCN) > 0, asc == TRUE, normDistMeanAbs <= closeDistance)

        toBeBought <- rbind(toBeBought, toBeBoughtByTrend)

        toBeBought %>%
          arrange(get(normDist2LBCN)) ->
          toBeBought
        #print(toBeBought)
        if (!is_empty(toBeBought) && nrow(toBeBought) > 0) {
          for (rowNumber in 1:nrow(toBeBought)) {
            row <- toBeBought[rowNumber, ]
            if (row$coin %in% bins$cc) {
              multipliers[multipliers$coin == row$coin,]$factor <- multipliers[multipliers$coin == row$coin,]$factor + 1
            }
            for (binsToFill in 1:filter(multipliers, coin == row$coin)$factor) {
              if (nrow(filter(bins, is.na(cc))) > 0) {
                firstEmptyBinIndex <-
                  filter(bins, is.na(cc))[1,]$binIndex

                boughtCoin <- row$coin
                availableInvestmentFunds <-
                  (1 - tradeBookCostRatio) * 0.998 * bins[firstEmptyBinIndex,]$reinvestmentBin
                boughtAmount <- availableInvestmentFunds / row$open
                bins[firstEmptyBinIndex, ]$cc <- boughtCoin
                bins[firstEmptyBinIndex, ]$amount <- boughtAmount
                bins[firstEmptyBinIndex, ]$price <- row$open
                bins[firstEmptyBinIndex, ]$reinvestmentBin <- 0
                bins[firstEmptyBinIndex, ]$lastInvestmentHeight <-
                  availableInvestmentFunds

                transactionHistory %>%
                  add_row(
                    cc = row$coin,
                    date = anytime(row$time),
                    amount = boughtAmount,
                    price = row$open,
                    value = availableInvestmentFunds,
                    type = "buy"
                  ) ->
                  transactionHistory
                # print(anytime(pointInTime))
                # print(paste("bought", boughtAmount, "of", row$coin, "for", row$high))
                # print(currentlyHeldCoins)
              }
            }
          }
        }
        currentlyHeldInvestment <- 0
        currentDaysData <- filter(data, time == pointInTime)
        for (i in 1:nrow(bins)) {
          coinName = bins[i, ]$cc
          amount = bins[i, ]$amount
          priceAtCurrentDay = filter(currentDaysData, coin == coinName)$open
          value = amount * priceAtCurrentDay
          currentlyHeldInvestment <- currentlyHeldInvestment + value + bins[i, ]$reinvestmentBin
        }
        if (is.numeric(currentlyHeldInvestment)) {
          if (length(currentlyHeldInvestment) >0) {
            investmentHistory[investmentHistory$time==pointInTime,]$investment <- currentlyHeldInvestment
          }
        }
      }
    }
    print(paste("held return ", currentlyHeldReturn))
    currentlyHeldInvestment <- 0
    lastTimestamp <- pointsInTime[nrow(pointsInTime), ]
    lastDaysData <- filter(data, time == lastTimestamp)
    for (i in 1:nrow(bins)) {
      coinName = bins[i, ]$cc
      amount = bins[i, ]$amount
      priceAtLastDay = filter(lastDaysData, coin == coinName)$open
      value = amount * priceAtLastDay
      currentlyHeldInvestment <- currentlyHeldInvestment + value + bins[i, ]$reinvestmentBin
    }
    print(paste("held investment ", currentlyHeldInvestment))
    print(paste("reserve ", reserveFund))

    d <- list()
    d$th <- transactionHistory
    d$ih <- investmentHistory
    return (d)
  }

require(dygraphs)
th <- win(data, 0.04, 10, 12, 1,0.1, 1.07, 0.01)
a <- xts(cbind(investment=th$ih$investment), order.by=anytime(th$ih$time))
dygraph(a)




#
# plot(x=anytime(ccData$date),
#      xlab="Date",
#      y=ccData$high,
#      ylim=y.range,
#      ylab="Price (BTC)",
#      type="l",
#      lwd=2)
# lines(x=anytime(ccData$date), y=get(avgCN, ccData), lty=2)
# lines(x=anytime(ccData$date), y=get(sd2upCN, ccData), col="gray40")
# lines(x=anytime(ccData$date), y=get(sd2downCN, ccData), col="gray40")
# lines(x=anytime(ccData$date), y=ccData$fiveUp, col="red", lty=2)
#
# xts(splinefun(data$date, data$high)(data$date, 1), data$date)
# a="high"
# filter(data, cc %in% c("ETH") & abs(get(a)) <0.05)
