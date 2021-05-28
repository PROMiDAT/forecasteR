calibrar.arima <- function(train, test, periodo, ar = 0:2, es = 0:1) {
  error.c <- Inf
  model.c <- NULL
  for (p in ar) {
    for (d in ar) {
      for (q in ar) {
        for (P in es) {
          for (D in es) {
            for (Q in es) {
              tryCatch({
                model.i <- arima(train, order = c(p, d, q), seasonal = list(
                  order = c(P, D, Q), period = periodo))
                pred.i  <- forecast(model.i, h = length(test))
                error.i <- RMSE(pred.i$mean, test)
                if(error.i < error.c) {
                  error.c <- error.i
                  model.c <- model.i         
                }
              }, error = function(e) {}, warning = function(w) {})
            }
          }
        }
      }
    }
  }
  
  return(model.c)
}
