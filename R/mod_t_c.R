#' t_c UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_t_c_ui <- function(id) {
  ns <- NS(id)
  
  opts_tc <- tabsOptions(heights = c(70, 50), tabs.content = list(
    list(
      options.run(), tags$hr(style = "margin-top: 0px;"),
      colourpicker::colourInput(
        ns("col_ts"), labelInput("colts"), "#5470c6", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_reg"), labelInput("coltend"), "#91cc75", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_fou"), labelInput("colcicl"), "#fac858", 
        allowTransparent = T)
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabTC"), opciones = opts_tc, title = NULL,
      tabPanel(title = labelInput("t_c"), 
               echarts4rOutput(ns('plot_tc'), height = "70vh"))
    )
  )
}
    
#' t_c Server Function
#'
#' @noRd 
mod_t_c_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  #' Gráfico de Tendencia y Ciclicidad
  output$plot_tc <- renderEcharts4r({
    serie   <- updateData$seriets
    datos   <- updateData$seriedf
    
    nombres <- c(tr("serie", updateData$idioma), tr("tend", updateData$idioma),
                 tr("cicl", updateData$idioma))
    col_ts  <- input$col_ts
    col_reg <- input$col_reg
    col_fou <- input$col_fou
    
    tryCatch({
      #cod <- code.cor(col_min, col_med, col_max)
      #updateAceEditor(session, "fieldCodeCor", value = cod)
      
      lserie <- log(serie)
      
      ini   <- datos[[1]][1]
      fin   <- datos[[1]][length(datos[[1]])]
      t     <- seq(as.numeric(ini), as.numeric(fin), length = length(serie))
      t2    <- t^2
      sin.t <- sin(2 * pi * t)
      cos.t <- cos(2 * pi * t)
      
      regresion <- lm(lserie ~ t + t2)$fit
      fourier   <- lm(lserie ~ t + t2 + sin.t + cos.t)$fit
      
      df <- data.frame(x = datos[[1]], w = lserie, y = regresion, z = fourier)
      
      df %>% e_charts(x) %>% e_line(w, name = nombres[1]) %>% 
        e_line(y, name = nombres[2], showSymbol = F) %>% 
        e_line(z, name = nombres[3], showSymbol = F) %>%
        e_datazoom(type = "slider") %>% e_x_axis(scale = T) %>%
        e_color(c(col_ts, col_reg, col_fou)) %>%
        e_y_axis(scale = T) %>% e_tooltip() %>% e_show_loading()
      
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_t_c_ui("t_c_ui_1")
    
## To be copied in the server
# callModule(mod_t_c_server, "t_c_ui_1")
 
