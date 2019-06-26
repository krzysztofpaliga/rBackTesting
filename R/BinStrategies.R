binStrategies <- list()

binStrategies$simpleBinStrategy <- list()

binStrategies$simpleBinStrategy$initBins <-
  function(numberOfBins = 20, initialInvestment) {
    initialBinInvestment <- initialInvestment / numberOfBins
    bins <-
      data_frame(
        binIndex = double(),
        coin = character(),
        amount = double(),
        price = double(),
        investmentHeight = double(),
        time = double()
      )
    for (i in 1:numberOfBins) {
      bins <-
        bins %>%
        add_row(
          binIndex = i,
          coin = NA,
          amount = NA,
          price = NA,
          investmentHeight = initialBinInvestment,
          time = NA
        )

    }
    return (bins)
  }
binStrategies$simpleBinStrategy$adjustBins <-
  function(bins, wallet) {
    return(list(bins, wallet))
  }

binStrategies$simpleBinStrategy$buy <-
  function(toBeBought,
           bins,
           wallet,
           transactionHistory,
           verbose = FALSE) {
    if (nrow(toBeBought) > 0) {
      for (i in 1:nrow(toBeBought)) {
        #print(paste("i ", i))
        tbbCoin <- toBeBought[i, ]$coin
        tbbOpen <- toBeBought[i, ]$open
        tbbClose <- toBeBought[i,]$close
        tbbTime <- toBeBought[i, ]$time
        slotBinIndex <-
          bins %>%
          filter(is.na(coin))
        if (nrow(slotBinIndex) > 0) {
          slotBinIndex <- slotBinIndex[1, ]$binIndex
        } else {
          # print(paste("missing slot for ", tbbCoin))
          next
        }

        bins[bins$binIndex == slotBinIndex, ]$coin = tbbCoin
        bins[bins$binIndex == slotBinIndex, ]$amount = (bins[bins$binIndex == slotBinIndex, ]$investmentHeight * 0.998) / (tbbClose* 1)
        bins[bins$binIndex == slotBinIndex, ]$price = tbbOpen
        bins[bins$binIndex == slotBinIndex, ]$time = tbbTime

        latestBoughtCoin <- bins[bins$binIndex == slotBinIndex, ]

        lbcCoin <- latestBoughtCoin$coin
        lbcTimestamp <- latestBoughtCoin$time
        lbcAmount <- latestBoughtCoin$amount
        lbcPrice <- latestBoughtCoin$price
        lbcValue <- lbcPrice * lbcAmount

        transactionHistory <- transactionHistory %>%
         add_row(
           coin = lbcCoin,
           amount = lbcAmount,
           type = "buy"
         )
      }
      if (verbose) {
        print("#### bins aftery buy")
        print(bins)
      }
    }
    # print(anytime(pointInTime))
    # print(paste("bought", boughtAmount, "of", row$coin, "for", row$high))
    # print(currentlyHeldCoins)


    return(list(bins, wallet, transactionHistory))
  }

binStrategies$simpleBinStrategy$sell <-
  function(toBeSold,
           bins,
           wallet,
           transactionHistory,
           verbose = FALSE) {
    if (nrow(toBeSold) > 0) {
      for (i in 1:nrow(toBeSold)) {
        coinToBeSold <- toBeSold[i, ]

        slotBinIndexes <-
          bins %>%
          filter(coin == coinToBeSold$coin) %>%
          select(binIndex)

        slotBinIndexes <- slotBinIndexes$binIndex

        if (!is.vector(slotBinIndexes)) {
          slotBinIndexes <- as.data.frame(slotBinIndexes)
        }
        for (i in 1:length(slotBinIndexes)) {
          slotBinIndex <- slotBinIndexes[i]
          if (is.na(slotBinIndex)) {
            break
          }
          bins[bins$binIndex == slotBinIndex, ]$investmentHeight <-
             bins[bins$binIndex == slotBinIndex, ]$amount * coinToBeSold$close * 0.998
          bins[bins$binIndex == slotBinIndex, ]$coin = NA
          bins[bins$binIndex == slotBinIndex, ]$amount = NA
          bins[bins$binIndex == slotBinIndex, ]$price = NA

          # transactionHistory <- transactionHistory %>%
          # add_row(
          #    coin = coinToBeSold$coin,
          #    amount = 1.0,
          #    type = "sell"
          #  )
        }
      }

      if (verbose) {
        print("#### bins after sell")
        print(bins)
      }
      # print(anytime(pointInTime))
      # print(paste("bought", boughtAmount, "of", row$coin, "for", row$high))
      # print(currentlyHeldCoins)


    }
    return(list(bins, wallet, transactionHistory))
  }


# Distributing bin strategy
binStrategies$distributingMeanBinStrategy$initBins <- binStrategies$simpleBinStrategy$initBins
binStrategies$distributingMeanBinStrategy$adjustBins <-
  function(bins, wallet) {

    meanInvestmentHeight <- mean(filter(bins, is.na(coin))$investmentHeight)

    binsNA <- bins %>%
      filter(is.na(coin))

    if (nrow(binsNA) > 0) {
      binsNA <- binsNA %>%
        mutate(investmentHeight = meanInvestmentHeight)
    }

    binsCoin <- bins %>%
      filter(!is.na(coin))

    bins <- rbind(binsNA, binsCoin)

    return(list(bins, wallet))
  }

binStrategies$distributingMeanBinStrategy$buy <- binStrategies$simpleBinStrategy$buy

binStrategies$distributingMeanBinStrategy$sell <- binStrategies$simpleBinStrategy$sell
