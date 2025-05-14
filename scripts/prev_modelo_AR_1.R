prev_modelo_AR_1 <- function(serie, h) {
  result <- serie %>% model(ar1 = AR(preco_m2 ~ order(1))) %>%
    forecast(h = h)
  return(result$.mean)
}
