prev_modelo_snaive <- function(serie, h) {
  result <- serie |> model(snaive = SNAIVE(valor ~ lag(12))) |>
    forecast(h = h)
  
  return(result$.mean)
}
