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
