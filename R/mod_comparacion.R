#' comparacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comparacion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxPreds"), #opciones = opc_disp, title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabEtable",
        withLoader(DT::dataTableOutput(ns('table_error')), 
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("txterror"), value = "tabEplot",
        echarts4rOutput(ns("plot_error"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabEpred",
        echarts4rOutput(ns("pred_error"), height = "70vh")
      )
    )
  )
}
    
#' comparacion Server Function
#'
#' @noRd 
mod_comparacion_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  output$table_error <- DT::renderDataTable({
    ms <- rvmodelo$ms
    res <- data.frame()
    
    for (m in ms) {
      if(!is.null(m)) {
        res <- rbind(res, m$error)
      }
    }
    
    DT::datatable(res, options = list(dom = 'frtip', scrollY = "50vh"))
  }, server = T)
  
  output$plot_error <- renderEcharts4r({
    ms <- rvmodelo$ms
    datos <- data.frame()
    for (m in ms) {
      if(!is.null(m)) {
        datos <- rbind(datos, m$error)
      }
    }
    
    grafico.errores(datos)
  })
  
  output$pred_error <- renderEcharts4r({
    ms <- rvmodelo$ms
    serie <- ts.union(isolate(updateData$train), isolate(updateData$test))
    
    nombres <- c()
    for (nombre in names(ms)) {
      if(!is.null(ms[[nombre]])) {
        serie <- ts.union(serie, ms[[nombre]]$pred)
        nombres <- c(nombres, nombre)
      }
    }
    
    serie <- data.frame(serie)
    names(serie) <- c("H", "O", nombres)
    serie$fecha  <- isolate(updateData$seriedf[[1]])
    
    res <- serie %>% e_charts(x = fecha) %>% e_datazoom() %>%
      e_tooltip(trigger = 'axis')
    
    for (s in names(serie)[-ncol(serie)]) {
      res <- res %>% e_line_(serie = s)
    }
    
    res
  })
}
    
## To be copied in the UI
# mod_comparacion_ui("comparacion_ui_1")
    
## To be copied in the server
# callModule(mod_comparacion_server, "comparacion_ui_1")
 
