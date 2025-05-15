prev_modelo_ARMA <- function(serie, h) {
  result <- serie %>% model(arma = ARIMA(valor ~ pdq(1, 0, 2))) %>% # melhor modelo escolhido iterando sobre um grid de p 0:4 e q 0:4
    forecast(h = h)
  return(result$.mean)
}
