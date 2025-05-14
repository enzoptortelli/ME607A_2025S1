prev_modelo_drift <- function(serie, h) {
  result <- serie |> model(drift = RW(valor ~ drift())) |>
    forecast(h = h)
  
  return(result$.mean)
}
