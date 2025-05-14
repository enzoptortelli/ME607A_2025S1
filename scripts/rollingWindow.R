library(stringr)

rollingWindow <- function(serie, n = 50, h = 3) {
  list_funcs <- list.files(path = 'scripts')
  funcs_previsao <- list_funcs[str_starts(list_funcs, pattern = 'prev_')]
  for(func_file in funcs_previsao) {
    source(str_glue('scripts/',func_file))
  }
  funcs_previsao <- funcs_previsao |> str_remove(pattern = '.R')
  
  num_previsoes <- nrow(serie) - n - h + 1
  
  ema_matrix <- matrix(0, nrow = length(funcs_previsao), ncol = h)
  colnames(ema_matrix) <- sapply(1:h, function(x) {
    return(str_glue('h={x}'))
  })
  rownames(ema_matrix) <- funcs_previsao |> str_remove('prev_modelo_')
  
  for(prev_func_name in funcs_previsao) {
    prev_func <- get(prev_func_name)
    res_matrix <- matrix(0, nrow = num_previsoes, ncol = h)
    for(i in 1:num_previsoes) {
      serie_window <- serie[i:(i + n - 1),]
      prev <- prev_func(serie_window, h)
      res_matrix[i,] <- abs(prev - serie$valor[(n+i):(n+i+h-1)])
    }
    ema_matrix[match(prev_func_name, funcs_previsao),] <- apply(res_matrix, MARGIN = 2, FUN = mean)
  }
  
  return(ema_matrix)
}
