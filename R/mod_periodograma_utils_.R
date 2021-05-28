#' Periodogram Data.frame
#'
#' @param x a ts object.
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export df_periods
#' @examples
#' df_periods(AirPassengers)
#' 
df_periods <- function(x) {
  res <- spec.pgram(x, log = "no", plot = F)
  res <- data.frame(spec = res$spec, freq = res$freq)
  res$per <- frequency(x) / res$freq
  res$pos.max <- order(res$spec, res$freq, decreasing = TRUE)
  res
}

#' Periodogram Plot
#'
#' @param x a ts object.
#' @param p which important period to plot.
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export plot_periods
#' @examples
#' plot_periods(AirPassengers)
#' 
plot_periods <- function(x, p = NULL) {
  df    <- df_periods(x)
  mejor <- df[df$pos.max[df$pos.max != 1][p], ]
  
  res <- df %>% e_charts(per) %>% e_line(spec) %>% e_datazoom(type = "slider") %>% 
    e_x_axis(scale = T) %>% e_y_axis(scale = T) %>% e_show_loading() %>% 
    e_legend(show = F) %>% e_tooltip(
      formatter = htmlwidgets::JS(paste0(
        "function(params){\n",
        "  return('Spec: ' + params.value[1].toFixed(2) + '<br/>Periodo: ' + params.value[0].toFixed(2))\n",
        "}"))
    )
  
  if(!is.null(p)) {
    if(p != 0) {
      res$x$opts$series[[2]] <- list(
        data = list(list(value = c(mejor$per, mejor$spec))), 
        type = "scatter", symbolSize = 15,
        labelLayout = list(
          y = 20, align = 'center', hideOverlap = T, moveOverlap = 'shiftX'
        ),
        labelLine = list(
          show = T, length2 = 5, lineStyle = list(width = 3, color = '#bbb')
        ),
        label = list(
          formatter = htmlwidgets::JS(paste0(
            "function(params){\n", 
            "  return 'El ", p, "º Periodo más importante es ", round(mejor$per, 2),
            "'\n}"
          )), show = T, minMargin = 10, position = 'top'
        )
      )
    }
  }
  
  return(res)
}
