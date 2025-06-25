prev_modelo_prophet <- function(serie, h) {
  
  lam <- BoxCox.lambda(serie$valor, method = 'loglik')
  valor_transformado <- BoxCox(serie$valor, lambda = lam)
  
  suppressWarnings({model_prophet <- prophet(tsibble(ds = serie$data, y = valor_transformado))})
  prophet_predict <- make_future_dataframe(model_prophet, periods = h, freq = 'month')
  predito <- predict(model_prophet, prophet_predict)$yhat %>% tail(n = 1)
  predito <- InvBoxCox(predito, lam)
  
  return(predito)
}
