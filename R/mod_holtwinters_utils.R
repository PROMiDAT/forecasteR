calibrar.HW <- function(serie.aprendizaje, serie.prueba, paso = 0.1) {
  error.c <- Inf
  alpha.i <- paso  # alpha no puede ser cero
  while(alpha.i <= 1) {
    beta.i <- 0
    while(beta.i <= 1) {
      gamma.i <- 0
      while(gamma.i <= 1) {
        tryCatch({
          mod.i <- HoltWinters(serie.aprendizaje, alpha = alpha.i, 
                               beta = beta.i, gamma = gamma.i)
          res.i <- forecast(mod.i, h = length(serie.prueba))
          error.i <- RMSE(res.i$mean, serie.prueba)
          if(error.i < error.c) {
            error.c <- error.i
            mod.c <- mod.i         
          }
        }, error = function(e) {}, warning = function(w) {})
        gamma.i <- gamma.i + paso
      }
      beta.i <- beta.i + paso
    }
    alpha.i <- alpha.i + paso
  }  
  return(mod.c)
}
