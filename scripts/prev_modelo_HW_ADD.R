prev_modelo_HW_ADD <- function(serie, h) {
  if('preco_m2' %in% colnames(serie)) serie$preco_m2 <- NULL
  
  result <- HoltWinters(serie, seasonal = 'add') |>
    forecast(h = h)
  
  return(result$mean |> as.vector())
}
