RSS <- function(Pred, Real) {
  return(sum((Real - Pred)^2))
}

MSE <- function(Pred, Real) {
  return((1/length(Real)) * RSS(Pred, Real))
}

RMSE <- function(Pred, Real) {
  return(sqrt(MSE(Pred, Real)))
}

PFA <- function(Pred, Real) {
  Total <- 0
  N <- length(Pred)
  for(i in 1:N) {
    if(Pred[i] > Real[i])
      Total <- Total + 1      
  }
  return(Total/N)
}

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

tabla.errores <- function(predicciones, real, nombres = NULL) {
  r <- data.frame()
  for (pred in predicciones) {
    r <- rbind(r, data.frame(
      'MSE' = MSE(pred, real), 'RMSE' = RMSE(pred, real),
      'PFA' = PFA(pred, real), 'PTFA' = PTFA(pred, real)
    )
    )
  }
  row.names(r) <- nombres
  return(r)
}

grafico.errores <- function (errores) {
  centros <- as.data.frame(apply(errores, 2, function(i)
    scales::rescale(i, to = c(0, 100))))
  
  res <- melt(t(centros), varnames = c("E", "Modelos"))
  res <- res[order(res$E, decreasing = F), ]
  res$M <- as.character(res$M)
  y = c(0, 25, 50, 75, 100)
  
  ggplot(res, aes(x = E, y = value, group = Modelos, color = Modelos, fill = Modelos)) +
    geom_polygon(alpha = 0.3, size = 1) + geom_point(size = 3) + 
    theme_minimal() + theme(axis.text.y = element_blank()) + xlab("") + 
    ylab("") + scale_y_continuous(limits = c(-10, 100), breaks = y) + 
    annotate("text", x = 0.5, y = y, label = paste0(y, "%"), color = "gray60") +
    ggproto("CordRadar", CoordPolar, theta = "x", r = "y", 
            start = 0, direction = sign(1))
}