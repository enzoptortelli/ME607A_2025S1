prev_modelo_HW_MUL <- function(serie, h) {
  if('preco_m2' %in% colnames(serie)) serie$preco_m2 <- NULL
  
  result <- HoltWinters(serie, seasonal = 'multiplicative') |>
    forecast(h = h)
  
  return(result$mean |> as.vector())
}
