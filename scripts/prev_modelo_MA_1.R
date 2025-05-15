prev_modelo_MA_1 <- function(serie, h) {
  result <- serie %>%
    model(arima = ARIMA(valor ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0))) %>%
    forecast(h = h)
  
  return(result$.mean)
}
