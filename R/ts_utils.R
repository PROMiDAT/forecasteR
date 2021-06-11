#' RSS
#'
#' @param Pred a ts object (prediction).
#' @param Real a ts object (real).
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export RSS
#' @examples
#' model <- arima(window(AirPassengers, end = c(1959, 12)))
#' pred  <- predict(model, 12)
#' RSS(pred$pred, window(AirPassengers, start = 1960))
#' 
RSS <- function(Pred, Real) {
  return(sum((Real - Pred)^2))
}

#' Mean Square Error
#'
#' @param Pred a ts object (prediction).
#' @param Real a ts object (real).
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export MSE
#' @examples
#' model <- arima(window(AirPassengers, end = c(1959, 12)))
#' pred  <- predict(model, 12)
#' MSE(pred$pred, window(AirPassengers, start = 1960))
#' 
MSE <- function(Pred, Real) {
  return((1/length(Real)) * RSS(Pred, Real))
}

#' Root Mean Square Error
#'
#' @param Pred a ts object (prediction).
#' @param Real a ts object (real).
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export RMSE
#' @examples
#' model <- arima(window(AirPassengers, end = c(1959, 12)))
#' pred  <- predict(model, 12)
#' RMSE(pred$pred, window(AirPassengers, start = 1960))
#' 
RMSE <- function(Pred, Real) {
  return(sqrt(MSE(Pred, Real)))
}

#' Percentage of Failures Up
#'
#' @param Pred a ts object (prediction).
#' @param Real a ts object (real).
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export PFA
#' @examples
#' model <- arima(window(AirPassengers, end = c(1959, 12)))
#' pred  <- predict(model, 12)
#' PFA(pred$pred, window(AirPassengers, start = 1960))
#' 
PFA <- function(Pred, Real) {
  Total <- 0
  N <- length(Pred)
  for(i in 1:N) {
    if(Pred[i] > Real[i])
      Total <- Total + 1      
  }
  return(Total/N)
}

#' Percentage of Total Failures Up
#'
#' @param Pred a ts object (prediction).
#' @param Real a ts object (real).
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric
#' @export PTFA
#' @examples
#' model <- arima(window(AirPassengers, end = c(1959, 12)))
#' pred  <- predict(model, 12)
#' PTFA(pred$pred, window(AirPassengers, start = 1960))
#' 
PTFA <- function(Pred, Real) {
  Total <- 0
  SReal <- 0
  for(i in 1:length(Pred)) {
    if(Pred[i] > Real[i]) {
      Total <- Total + (Pred[i] - Real[i])
      SReal <- SReal + abs(Real[i])
    }
  }
  if(Total == 0)
    SReal = 1
  return(Total/SReal)
}

#' Error table for all predictions
#'
#' @param Preds a list of ts objects (prediction).
#' @param Real a ts object (real).
#' @param nombres names for the data.frame (optional).
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export tabla.errores
#' @examples
#' model <- arima(window(AirPassengers, end = c(1959, 12)))
#' pred  <- predict(model, 12)
#' tabla.errores(list(pred$pred), window(AirPassengers, start = 1960))
#' 
tabla.errores <- function(Preds, Real, nombres = NULL) {
  r <- data.frame()
  for (pred in Preds) {
    r <- rbind(r, data.frame(
      'MSE' = MSE(pred, Real), 'RMSE' = RMSE(pred, Real),
      'PFA' = PFA(pred, Real), 'PTFA' = PTFA(pred, Real)
    )
    )
  }
  row.names(r) <- nombres
  return(r)
}

#' Error plot for all predictions
#'
#' @param errores a data.frame with errors of a time series.
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export grafico.errores
#' @import echarts4r
#' @importFrom scales rescale
#' @examples
#' model <- arima(window(AirPassengers, end = c(1959, 12)))
#' pred  <- predict(model, 12)
#' e <- tabla.errores(list(pred$pred), window(AirPassengers, start = 1960))
#' grafico.errores(e)
#' 
grafico.errores <- function (errores) {
  errores <- apply(errores, 2, function(i) scales::rescale(i, to = c(0, 100)))
  errores <- errores + 10
  errores <- data.frame(t(errores))
  errores$vars <- row.names(errores)
  
  res <- errores %>% e_charts(vars)
  
  text_tooltip <- ""
  for (i in 1:nrow(errores)) {
    text_tooltip <- paste0(text_tooltip, "'<br/>", errores$vars[i], 
                           ": ' + (params.value[", i-1, "]-10).toFixed(3)")
    if(i < nrow(errores)) text_tooltip <- paste0(text_tooltip, " + ")
  }
  
  for (i in 1:(ncol(errores) - 1)) {
    res <- res %>% 
      e_radar_(colnames(errores)[i], name = colnames(errores)[i], max = 110,
               areaStyle = list())
  }
  
  res %>% e_show_loading() %>%
    e_tooltip(
      formatter = htmlwidgets::JS(paste0(
        "function(params) {
          return(params.name + ", text_tooltip, ")
        }"
      )), trigger = "item"
    )
}

############################### Generar Código ################################
code.plots <- function(noms, colors) {
  paste0(
    "serie           <- data.frame(ts.union(train, test, ", noms[4], "))\n",
    "serie$date      <- seriedf[[1]]\n",
    "colnames(serie) <- c('train', 'test', 'pred', 'date')\n\n",
    "serie %>% e_charts(x = date) %>%\n", 
    "  e_line(serie = train, name = '", noms[1], "') %>%\n",
    "  e_line(serie = test,  name = '", noms[2], "') %>%\n",
    "  e_line(serie = pred,  name = '", noms[3], "') %>%\n",
    "  e_datazoom() %>% e_tooltip(trigger = 'axis') %>% e_show_loading() %>%\n",
    "  e_color(c('", paste(colors, collapse = "','"), "'))"
  )
}
