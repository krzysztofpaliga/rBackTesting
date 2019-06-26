win <-
  function(data,
           windowSize,
           tradeBookCostRatio,
           binStrategy,
           buyStrategy,
           sellStrategy,
           numberOfBins,
           initialInvestment,
           prophet) {

    transactionHistory <-
      data_frame(
        coin = character(),
        amount = double(),
        type = character()
      )

    wallet <- list()
    wallet$reserveFund <- 0
    wallet$currentlyHeldReturn <- 0


    meanCN <- paste("avg", windowSize, sep="_")

    bins <- binStrategy$initBins(numberOfBins, initialInvestment)

    pointsInTime <- distinct(data, time) %>% ungroup() %>% distinct(time) %>% arrange(time)

    for (pointInTimeRowNumber in 1:nrow(pointsInTime)) {
    #  pointInTimeRowNumber = 1
      print(paste("poin in time ", pointInTimeRowNumber))
      print(paste("held investment ", sum(bins$investmentHeight)))
      pointInTime <- pointsInTime[pointInTimeRowNumber, ]$time

      dataAtPointInTime <- filter(data, time == pointInTime)

      toBeSold <- sellStrategy$copyWhatIsToBeSold(dataAtPointInTime, prophet, bins, verbose = FALSE)

      returned <- binStrategy$sell(toBeSold, bins = bins, wallet, transactionHistory, verbose = FALSE)
      bins <- returned[[1]]
      wallet <- returned[[2]]
      transactionHistory <- returned[[3]]

      returned <- binStrategy$adjustBins(bins, wallet)
      bins <- returned[[1]]
      wallet <- returned[[2]]

      toBeBought <- buyStrategy$copyWhatIsTeBeBought(dataAtPointInTime = dataAtPointInTime, prophet, verbose = FALSE)

      returned <- binStrategy$buy(toBeBought = toBeBought, bins = bins, wallet = wallet, transactionHistory = transactionHistory, verbose = FALSE)
      bins <- returned[[1]]
      wallet <- returned[[2]]
      transactionHistory <- returned[[3]]
    }

    currentlyHeldCash <- 0
    currentlyHeldInvestment <- 0
    lastTimestamp <- pointsInTime[nrow(pointsInTime), ]
    lastDaysData <- filter(data, time == lastTimestamp)
    for (i in 1:nrow(bins)) {
      print(paste("i ", i))
      if (is.na(bins[i,]$coin)) {
        currentlyHeldCash <- currentlyHeldCash + bins[i,]$investmentHeight
      } else {
        coinName = bins[i, ]$coin
        amount = bins[i, ]$amount
        priceAtLastDay = filter(lastDaysData, coin == coinName)$open
        if (is_empty(priceAtLastDay)) {
          print(paste("coin ", coinName, " not more traded"))
        } else {
        value = amount * priceAtLastDay
        if (is.numeric(value)) {
          currentlyHeldInvestment <- currentlyHeldInvestment + value
        }
        }
      }
    }
    print(paste("held investment ", currentlyHeldInvestment))
    print(paste("held cash ", currentlyHeldCash))
    print(bins)
    print(paste("investment height ", sum(bins$investmentHeight)))
    return(transactionHistory)
  }

winRollingWindow <-
  function(data,
           dataBuy,
           dataSell,
           windowSize,
           tradeBookCostRatio,
           binStrategy,
           buyStrategy,
           sellStrategy,
           numberOfBins,
           initialInvestment) {

    transactionHistory <-
      data_frame(
        date = as.POSIXct(character()),
        coin = character(),
        amount = double(),
        price = double(),
        value = double(),
        type = character()
      )

    wallet <- list()
    wallet$reserveFund <- 0
    wallet$currentlyHeldReturn <- 0


    meanCN <- paste("avg", windowSize, sep="_")

    bins <- binStrategy$initBins(numberOfBins, initialInvestment)

    pointsInTime <- distinct(data, time) %>% ungroup() %>% distinct(time) %>% arrange()

    for (pointInTimeRowNumber in 1:nrow(pointsInTime)) {
      #pointInTimeRowNumber = 521
      print(paste("poin in time ", pointInTimeRowNumber))
      print(paste("held investment ", sum(bins$investmentHeight)))
      pointInTime <- pointsInTime[pointInTimeRowNumber, ]$time

      dataAtPointInTimeBuy <- filter(dataBuy, time == pointInTime)
      dataAtPointInTimeSell <- filter(dataSell, time == pointInTime)

      toBeSold <- sellStrategy$copyWhatIsToBeSold(dataAtPointInTimeSell, bins, verbose = FALSE)

      returned <- binStrategy$sell(toBeSold, bins = bins, wallet, transactionHistory, verbose = FALSE)
      bins <- returned[[1]]
      wallet <- returned[[2]]
      transactionHistory <- returned[[3]]

      returned <- binStrategy$adjustBins(bins, wallet)
      bins <- returned[[1]]
      wallet <- returned[[2]]

      toBeBought <- buyStrategy$copyWhatIsTeBeBought(dataAtPointInTimeBuy, verbose = FALSE)

      returned <- binStrategy$buy(toBeBought = toBeBought, bins = bins, wallet = wallet, transactionHistory = transactionHistory, verbose = FALSE)
      bins <- returned[[1]]
      wallet <- returned[[2]]
      transactionHistory <- returned[[3]]
    }

    currentlyHeldCash <- 0
    currentlyHeldInvestment <- 0
    lastTimestamp <- pointsInTime[nrow(pointsInTime), ]
    lastDaysData <- filter(data, time == lastTimestamp)
    for (i in 1:nrow(bins)) {
      print(paste("i ", i))
      if (is.na(bins[i,]$coin)) {
        currentlyHeldCash <- currentlyHeldCash + bins[i,]$investmentHeight
      } else {
        coinName = bins[i, ]$coin
        amount = bins[i, ]$amount
        priceAtLastDay = filter(lastDaysData, coin == coinName)$open
        if (is_empty(priceAtLastDay)) {
          print(paste("coin ", coinName, " not more traded"))
        } else {
          value = amount * priceAtLastDay
          if (is.numeric(value)) {
            currentlyHeldInvestment <- currentlyHeldInvestment + value
          }
        }
      }
    }
    print(paste("held investment ", currentlyHeldInvestment))
    print(paste("held cash ", currentlyHeldCash))
    print(bins)
    print(paste("investment height ", sum(bins$investmentHeight)))
    return(bins)
  }
