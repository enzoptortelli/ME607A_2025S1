prev_modelo_AR_2 <- function(serie, h) {
  result <- serie %>% model(ar2 = AR(valor ~ order(2))) %>%
    forecast(h = h)
  return(result$.mean)
}
