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
