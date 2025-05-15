prev_modelo_MA_2 <- function(serie, h) {
  result <- serie %>%
    model(arima = ARIMA(valor ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0))) %>%
    forecast(h = h)
  
  return(result$.mean)
}
