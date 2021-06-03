#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 20, -1), iDisplayLength = 10,
      scrollX = TRUE, language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'),
        info = "", emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  
  ##################################  Variables  ##############################
  updateData <- rv(datos = NULL, seriedf = NULL, seriets = NULL, train = NULL,
                   test = NULL, ts_type = NULL, idioma = NULL)
  
  rvmodelo <- rv(ms = list(prom = NULL, inge = NULL, eing = NULL, drif = NULL,
                           deco = NULL, holt = NULL, arim = NULL))
  
  ###################################  Update  ################################
  #' Update on Language
  observeEvent(input$idioma, {
    updateData$idioma <- input$idioma
    updateLabelInput(session, cambiar.labels(), tr(cambiar.labels(), input$idioma))
  })
  
  #' Enable/disable on load data
  observe({
    element <- "#sidebarItemExpanded li"
    menu.values <- c("[class^=treeview]", " a[data-value=comp]")
    
    lapply(menu.values, function(i) {
      if(is.null(updateData$seriets)) {
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
      }
    })
  })
  
  ###################################  Modules  ###############################
  callModule(mod_carga_datos_server,  "carga_datos_ui_1",  updateData, rvmodelo)
  callModule(mod_normal_server,       "normal_ui_1",       updateData)
  callModule(mod_t_c_server,          "t_c_ui_1",          updateData)
  callModule(mod_descom_server,       "descom_ui_1",       updateData)
  callModule(mod_periodograma_server, "periodograma_ui_1", updateData)
  
  callModule(mod_promedio_server,    "promedio_ui_1",    updateData, rvmodelo)
  callModule(mod_ingenuo_server,     "ingenuo_ui_1",     updateData, rvmodelo)
  callModule(mod_e_ingenuo_server,   "e_ingenuo_ui_1",   updateData, rvmodelo)
  callModule(mod_desvio_server,      "desvio_ui_1",      updateData, rvmodelo)
  callModule(mod_m_descom_server,    "m_descom_ui_1",    updateData, rvmodelo)
  callModule(mod_redes_server,       "redes_ui_1",       updateData, rvmodelo)
  callModule(mod_holtwinters_server, "holtwinters_ui_1", updateData, rvmodelo)
  callModule(mod_arima_server,       "arima_ui_1",       updateData, rvmodelo)
  callModule(mod_comparacion_server, "comparacion_ui_1", updateData, rvmodelo)
  callModule(mod_nuevos_server,      "nuevos_ui_1",      updateData)
}
