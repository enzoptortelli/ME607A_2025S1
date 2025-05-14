prev_modelo_mean <- function(serie, h) {
  return(rep(mean(serie$valor), h))
}

