prev_modelo_naive <- function(serie, h) {
  return(rep(tail(serie$valor, n = 1), h))
}
