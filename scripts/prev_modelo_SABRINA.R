prev_modelo_SABRINA <- function(serie, h) {
  result <- serie %>%
    model(sarima = ARIMA(valor)) %>%  # auto SARIMA
    forecast(h = h)
  
  return(result$.mean)
}