#' promedio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom forecast forecast meanf
mod_promedio_ui <- function(id) {
  ns <- NS(id)
  
  opc_prom <- tabsOptions(heights = c(70, 50), tabs.content = list(
    list(
      options.run(ns("run_prom")), tags$hr(style = "margin-top: 0px;"),
      conditionalPanel(
        condition = "input.BoxProm == 'tabPlot'", ns = ns,
        colourpicker::colourInput(
          ns("col_train_prom"), labelInput("coltrain"), "#5470c6", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_test_prom"), labelInput("coltest"), "#91cc75", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_p_prom"), labelInput("colpred"), "#fac858", 
          allowTransparent = T)
      )
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxProm"), opciones = opc_prom, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_prom")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_prom'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_prom"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_prom"))
      )
    )
  )
}
    
#' promedio Server Function
#'
#' @noRd 
mod_promedio_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxProm, {
    if(input$BoxProm == "tabText") {
      shinyjs::show('run_prom')
    } else {
      shinyjs::hide('run_prom')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxProm", selected = "tabText")
  })
  
  output$text_prom <- renderPrint({
    input$run_prom
    train <- updateData$train
    test  <- updateData$test
    
    modelo <- meanf(train, h = length(test))
    pred   <- modelo$mean
    isolate(rvmodelo$ms$prom$model <- modelo)
    isolate(rvmodelo$ms$prom$pred  <- pred)
    isolate(rvmodelo$ms$prom$error <- tabla.errores(list(pred), test, c("prom")))
    
    modelo
  })
  
  output$table_prom <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$ms$prom$pred, 
                      abs(seriedf[[2]] - rvmodelo$ms$prom$pred))
    colnames(res) <- c(tr('date', lg), "Real", tr('table_m', lg), 
                       tr('diff', lg))
    
    DT::datatable(
      res, selection = 'none', editable = F, rownames = F, 
      options = list(dom = 'frtp', scrollY = "50vh")
    )
  })
  
  output$plot_prom <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$ms$prom$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$fecha <- isolate(updateData$seriedf[[1]])
    colnames(serie) <- c("train", "test", "pred", "fecha")
    colores <- c(input$col_train_prom, input$col_test_prom, input$col_p_prom)
    
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
  
  output$error_prom <- renderUI({
    lg <- updateData$idioma
    
    div(
      style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
      infoBox(tr("mse", lg), rvmodelo$ms$prom$error$MSE, NULL, 
              icon("warning"), "red", 6, fill = T),
      infoBox(tr("rmse", lg), rvmodelo$ms$prom$error$RMSE, NULL, 
              icon("warning"), "yellow", 6, fill = T),
      infoBox(tr("pfa", lg), rvmodelo$ms$prom$error$PFA, NULL, 
              icon("level-up"), "green", 6, fill = T),
      infoBox(tr("ptfa", lg), rvmodelo$ms$prom$error$PTFA, NULL, 
              icon("level-up"), "navy", 6, fill = T)
    )
  })
}
    
## To be copied in the UI
# mod_promedio_ui("promedio_ui_1")
    
## To be copied in the server
# callModule(mod_promedio_server, "promedio_ui_1")
 
