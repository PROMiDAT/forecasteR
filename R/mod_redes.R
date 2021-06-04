#' redes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom forecast forecast nnetar
mod_redes_ui <- function(id){
  ns <- NS(id)
  
  opc_redes <- tabsOptions(heights = c(70, 50), tabs.content = list(
    list(
      options.run(ns("run_redes")), tags$hr(style = "margin-top: 0px;"),
      conditionalPanel(
        condition = "input.BoxRedes == 'tabText'", ns = ns,
        numericInput(ns("tam"), labelInput("tamred"), 10, min = 0, step = 5)
      ),
      conditionalPanel(
        condition = "input.BoxRedes == 'tabPlot'", ns = ns,
        colourpicker::colourInput(
          ns("col_train_redes"), labelInput("coltrain"), "#5470c6", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_test_redes"), labelInput("coltest"), "#91cc75", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_p_redes"), labelInput("colpred"), "#fac858", 
          allowTransparent = T)
      )
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxRedes"), opciones = opc_redes, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_redes")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_redes'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_redes"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_redes"))
      )
    )
  )
}
    
#' redes Server Function
#'
#' @noRd 
mod_redes_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxRedes, {
    if(input$BoxRedes == "tabText") {
      shinyjs::show('run_redes')
    } else {
      shinyjs::hide('run_redes')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxRedes", selected = "tabText")
  })
  
  output$text_redes <- renderPrint({
    input$run_redes
    train <- updateData$train
    test  <- updateData$test
    tam   <- isolate(input$tam)
    
    modelo <- nnetar(train, size = tam)
    pred   <- forecast(modelo, h = length(test))$mean
    isolate(rvmodelo$ms$redes$model <- modelo)
    isolate(rvmodelo$ms$redes$pred  <- pred)
    isolate(rvmodelo$ms$redes$error <- tabla.errores(list(pred), test, c("redes")))
    
    rvmodelo$ms$redes$model
  })
  
  output$table_redes <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$ms$redes$pred, 
                      abs(seriedf[[2]] - rvmodelo$ms$redes$pred))
    colnames(res) <- c(tr('date', lg), "Real", tr('table_m', lg), 
                       tr('diff', lg))
    
    DT::datatable(
      res, selection = 'none', editable = F, rownames = F, 
      options = list(dom = 'frtp', scrollY = "50vh")
    )
  })
  
  output$plot_redes <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$ms$redes$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$fecha <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "fecha")
    colores <- c(input$col_train_redes, input$col_test_redes, input$col_p_redes)
    
    tryCatch({
      serie %>% e_charts(x = fecha) %>% 
        e_line(serie = train, name = tr('train', lg)) %>%
        e_line(serie = test,  name = tr('test', lg)) %>% 
        e_line(serie = pred,  name = tr('table_m', lg)) %>% 
        e_datazoom() %>% e_tooltip(trigger = 'axis') %>% e_show_loading() %>%
        e_color(colores)
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$error_redes <- renderUI({
    lg <- updateData$idioma
    
    div(
      style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
      infoBox(tr("mse", lg), rvmodelo$ms$redes$error$MSE, NULL, 
              icon("warning"), "red", 6, fill = T),
      infoBox(tr("rmse", lg), rvmodelo$ms$redes$error$RMSE, NULL, 
              icon("warning"), "yellow", 6, fill = T),
      infoBox(tr("pfa", lg), rvmodelo$ms$redes$error$PFA, NULL, 
              icon("level-up"), "green", 6, fill = T),
      infoBox(tr("ptfa", lg), rvmodelo$ms$redes$error$PTFA, NULL, 
              icon("level-up"), "navy", 6, fill = T)
    )
  })
}
    
## To be copied in the UI
# mod_redes_ui("redes_ui_1")
    
## To be copied in the server
# callModule(mod_redes_server, "redes_ui_1")
 
