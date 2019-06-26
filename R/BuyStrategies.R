buyStrategies <- list()

buyStrategies$cc <- list()

buyStrategies$cc$hammerBuyStrategy <- list()
buyStrategies$cc$hammerBuyStrategy$copyWhatIsTeBeBought <- function(dataAtPointInTime, verbose = FALSE) {
  stopifnot(is.logical(dataAtPointInTime$ascBuy))
  stopifnot(is.numeric(dataAtPointInTime$csNormBody))
  stopifnot(is.numeric(dataAtPointInTime$csNormUShadow))
  stopifnot(is.numeric(dataAtPointInTime$csNormLShadow))

  toBeBought <- dataAtPointInTime %>%
    #filter(desc == TRUE, csLShadow > 2 * csBody, csUShadow < csBody) %>%
    filter(ascBuy == FALSE, csLShadow > 2 * csBody, csUShadow < csBody) %>%
    arrange(desc(csLShadow), csUShadow, csBody) %>%
    select(coin, open, close, high, low, time)


  if (verbose && nrow(toBeBought) > 0) {
    print(toBeBought)
  }
  return(toBeBought)
}

buyStrategies$prophet <- list()
buyStrategies$prophet$buyUndervalued <-list()
buyStrategies$prophet$buyUndervalued$copyWhatIsToBeBought <- function(dataAtPointInTime, objs, bins, verbose = FALSE) {
  ccnamesd <- dataAtPointInTime %>% distinct(coin)
  ccnames <- ccnamesd$coin
  timestampd <- dataAtPointInTime %>% distinct(time)
  timestamp <- timestampd$time
  lastPointDifferences <- gatherAllLastPointDifferences(objs = objs, ccnames = ccnames, timestamp = timestamp)
  lastPointDifferences <- lastPointDifferences %>% filter(!is.na(lpd))
  if (nrow(lastPointDifferences) > 0) {
    lastPointDifferences <- arrange(lastPointDifferences, desc(lpd))
    best <- lastPointDifferences %>% filter(lpd > 0, ccname %ni% bins$coin)
    toBeBoughtD <- dataAtPointInTime %>%
      filter(coin %in% best$ccname)
    if (nrow(toBeBoughtD) > 0) {
      toBeBought <- toBeBoughtD %>% select(coin, open, close, high, low, time)
      return(toBeBought)
    }
  }
  return(data.frame())
}


getLastPointDifference <- function (objs, ccname, timestamp)
{
  lastIndex <- getIndexFromObjs(objs = objs, ccname = ccname, timestampa = timestamp)
  lastRealPoint <- objs[[ccname]]$m$history$y[lastIndex]
  lastDate <- objs[[ccname]]$m$history$ds[lastIndex]
  lastYhatL <- objs[[ccname]]$forecast$yhat_lower[lastIndex]
  return ((lastYhatL - lastRealPoint) / lastRealPoint)
}

gatherAllLastPointDifferences <- function (objs, ccnames, timestamp)
{
  lastPointDifferences <- c()
  for (i in 1:length(ccnames[]))
  {
    ccname <- ccnames[i]
    lastPointDifferences[i] <- getLastPointDifference(objs, ccname, timestamp)
  }

  df <- data.frame(ccnames, lastPointDifferences)
  colnames(df) <- c("ccname", "lpd")

  return (df)
}

getHighestTrendDifference <- function (obj, timestamp)
{
  lastIndex <- getIndexFromObj(obj = obj, timestamp = timestamp)
  lastRealPoint <- obj$m$history$y[lastIndex]
  maxFromTrendPrediction <- max(obj$forecast$yhat[c(lastIndex+1,lastIndex+2,lastIndex+3)])
  return ((maxFromTrendPrediction - lastRealPoint) / lastRealPoint)
}

gatherAllHighestTrendDifferences <- function (df, objs, ccnames, timestamp)
{
  highestTrendDifferences <- c()
  for (i in 1:length(ccnames[]))
  {
    ccname <- ccnames[i]
    highestTrendDifferences[i] <- getHighestTrendDifference(objs[[ccname]], timestamp)
  }
  df$htd <- highestTrendDifferences

  return (df)
}

plotd <- function (objs, ccname)
{
  dyplot.prophet(objs[[ccname]]$m, objs[[ccname]]$forecast)
}

getIndexFromObjs <- function (objs, ccname, timestampa)
{
  history <- objs[[ccname]]$m$history
  history$rownames <- rownames(history)
  index <- filter(history, timestamp == timestampa)$rownames

  return(as.numeric(index))
}

getIndexFromObj <- function (obj, timestampa)
{
  history <- obj$m$history
  history$rownames <- rownames(history)
  index <- filter(history, timestamp == timestampa)$rownames

  return(as.numeric(index))
}
