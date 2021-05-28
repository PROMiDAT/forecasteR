#' Decompose plot
#'
#' @param serie a ts object.
#' @param f vector of dates for the time series.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_decompose
#' @import echarts4r
#' @importFrom stats decompose
#' @examples
#' e_decompose(AirPassengers)
#' 
e_decompose <- function(serie, f = NULL) {
  if(is.null(f)) {
    f <- 1:length(serie)
  }
  d <- decompose(serie)
  d <- data.frame(f, v = d$x, t = d$trend, s = d$seasonal, r = d$random)
  
  opts <- list(
    xAxis = list(
      list(data = f, gridIndex = 0),
      list(data = f, gridIndex = 1),
      list(data = f, gridIndex = 2),
      list(data = f, gridIndex = 3)
    ),
    yAxis = list(
      list(name = "History", nameTextStyle = list(fontWeight = 'bold')),
      list(gridIndex = 1, name = "Trend", 
           nameTextStyle = list(fontWeight = 'bold')),
      list(gridIndex = 2, name = "Seasonal", 
           nameTextStyle = list(fontWeight = 'bold')),
      list(gridIndex = 3, name = "Random", 
           nameTextStyle = list(fontWeight = 'bold'))
    ),
    grid = list(
      list(bottom = '78%'),
      list(top = '30%', bottom = '54%'),
      list(top = '54%', bottom = '32%'),
      list(top = '76%')
    ),
    series = list(
      list(type = "line", data = d$v),
      list(type = "line", data = d$t, xAxisIndex = 1, yAxisIndex = 1),
      list(type = "line", data = d$s, xAxisIndex = 2, yAxisIndex = 2),
      list(type = "line", data = d$r, xAxisIndex = 3, yAxisIndex = 3)
    ),
    tooltip = list(
      trigger = 'axis'
    ),
    dataZoom = list(
      xAxisIndex = c(0, 1, 2, 3)
    ),
    axisPointer = list(
      link = list(xAxisIndex = 'all')
    )
  )
  
  res <- e_charts() %>% e_list(opts) %>% e_show_loading()
  return(res)
}