sellStrategies <- list()

sellStrategies$cc <- list()
sellStrategies$cc$hammerSellStrategy <- list()
sellStrategies$cc$hammerSellStrategy$copyWhatIsToBeSold<- function (dataAtPointInTime, bins, verbose = FALSE) {
  stopifnot(is.logical(dataAtPointInTime$ascSell))
  stopifnot(is.numeric(dataAtPointInTime$csNormBody))
  stopifnot(is.numeric(dataAtPointInTime$csNormUShadow))
  stopifnot(is.numeric(dataAtPointInTime$csNormLShadow))

  toBeSold <- dataAtPointInTime %>%
    #filter(coin %in% bins$coin, asc == TRUE, csNormLShadow > 2 * csNormBody, csNormUShadow < csNormBody) %>%
    filter(coin %in% bins$coin, ascSell == TRUE, csLShadow > 2 * csBody,  csUShadow < csBody) %>%
    select(coin, open, close, high, low, time)


  if (verbose && nrow(toBeSold) > 0) {
    print(toBeSold)
  }
  return(toBeSold)
}

sellStrategies$prophet <- list()
sellStrategies$prophet$sellOvervalued <-list()
sellStrategies$prophet$sellOvervalued$copyWhatIsToBeSold <- function(dataAtPointInTime, objs, bins, verbose = FALSE) {
  ccnamesd <- dataAtPointInTime %>% distinct(coin)
  ccnamesa <- levels(factor(ccnamesd$coin))
  timestampd <- dataAtPointInTime %>% distinct(time)
  timestamp <- timestampd$time
  lastPointDifferences <- gatherAllLastPointDifferencesS(objs = objs, ccnamesa = ccnamesa, timestamp = timestamp)
  lastPointDifferences <- lastPointDifferences %>% filter(!is.na(lpd))
  if (nrow(lastPointDifferences) > 0) {
    overvalued <- lastPointDifferences %>% filter(lpd>0)
    toBeSold <- dataAtPointInTime %>%
      filter(coin %in% overvalued$ccname) %>%
      select(coin, open, close, high, low, time)
    return(toBeSold)
  }
  return(data.frame())
}


getLastPointDifferenceS <- function (objs, ccname, timestamp)
{
  lastIndex <- getIndexFromObjsS(objs = objs, ccnamea = ccname, timestampa = timestamp)
  lastRealPoint <- objs[[ccname]]$m$history$y[lastIndex]
  lastDate <- objs[[ccname]]$m$history$ds[lastIndex]
  lastYhatU <- objs[[ccname]]$forecast$yhat_upper[lastIndex]
  return ((lastYhatU - lastRealPoint) / lastRealPoint)
}

gatherAllLastPointDifferencesS <- function (objs, ccnamesa, timestamp)
{
  lastPointDifferences <- c()
  for (i in 1:length(ccnamesa[]))
  {
    ccname <- ccnamesa[i]
    lastPointDifferences[i] <- getLastPointDifferenceS(objs = objs, ccname = ccname, timestamp = timestamp)
  }

  df <- data.frame(ccnamesa, lastPointDifferences)
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

getIndexFromObjsS <- function (objs, ccnamea, timestampa)
{
  history <- objs[[ccnamea]]$m$history
  history$rownames <- rownames(history)
  index <- filter(history, timestamp == timestampa)$rownames

  return(as.numeric(index))
}

getIndexFromObjS <- function (obj, timestampa)
{
  history <- obj$m$history
  history$rownames <- rownames(history)
  index <- filter(history, timestamp == timestampa)$rownames

  return(as.numeric(index))
}
