#' descom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_descom_ui <- function(id){
  ns <- NS(id)
  
  opts_descom <- tabsOptions(heights = c(70, 50), tabs.content = list(
    list(
      options.run(), tags$hr(style = "margin-top: 0px;"),
      colourpicker::colourInput(
        ns("col_hist"), labelInput("colts"), "#5470c6", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_tend"), labelInput("coltend"), "#91cc75", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_seas"), labelInput("colseas"), "#fac858", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_resi"), labelInput("colresi"), "#ef6566", 
        allowTransparent = T)
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabDescom"), opciones = opts_descom, title = NULL,
      tabPanel(title = labelInput("desc"), 
               echarts4rOutput(ns('plot_descom'), height = "70vh"))
    )
  )
}
    
#' descom Server Function
#'
#' @noRd 
mod_descom_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  #' Gráfico de Descomposición
  output$plot_descom <- renderEcharts4r({
    serie <- updateData$seriets
    datos <- updateData$seriedf
    
    col_hist <- input$col_hist
    col_tend <- input$col_tend
    col_seas <- input$col_seas
    col_resi <- input$col_resi
    
    lg    <- updateData$idioma
    noms  <- c(tr('serie', lg), tr('tend', lg), tr('seas', lg), tr('resi', lg))
    
    e_decompose(serie, datos[[1]], noms) %>% 
      e_color(c(col_hist, col_tend, col_seas, col_resi))
  })
}
    
## To be copied in the UI
# mod_descom_ui("descom_ui_1")
    
## To be copied in the server
# callModule(mod_descom_server, "descom_ui_1")
 
