#' holtwinters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_holtwinters_ui <- function(id){
  ns <- NS(id)
  
  opc_holt <- tabsOptions(heights = c(80, 50), tabs.content = list(
    list(
      options.run(ns("run_holt")), tags$hr(style = "margin-top: 0px;"),
      conditionalPanel(
        condition = "input.BoxHolt == 'tabText'", ns = ns,
        fluidRow(
          col_4(numericInput(ns('alpha'), 'alpha', 1, 0, 1, 0.05)),
          col_4(numericInput(ns('beta'),  'beta',  0, 0, 1, 0.05)),
          col_4(numericInput(ns('gamma'), 'gamma', 0, 0, 1, 0.05))
        ), tags$hr(style = "margin-top: 0px;"),
        h4(labelInput('calip')), tags$hr(style = "margin-top: 0px;"),
        numericInput(ns('paso'), labelInput('paso'), 0.1, 0, 1, 0.05),
        actionButton(ns('calhw'), labelInput('cali'), width = '100%')
      ),
      conditionalPanel(
        condition = "input.BoxHolt == 'tabPlot'", ns = ns,
        colourpicker::colourInput(
          ns("col_train_holt"), labelInput("coltrain"), "#5470c6", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_test_holt"), labelInput("coltest"), "#91cc75", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_p_holt"), labelInput("colpred"), "#fac858", 
          allowTransparent = T)
      )
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxHolt"), opciones = opc_holt, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_holt")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_holt'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_holt"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_holt"))
      )
    )
  )
}
    
#' holtwinters Server Function
#'
#' @noRd 
mod_holtwinters_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  v <- rv(checkhw = 0)
  
  observeEvent(input$BoxHolt, {
    if(input$BoxHolt == "tabText") {
      shinyjs::show('run_holt')
    } else {
      shinyjs::hide('run_holt')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxHolt", selected = "tabText")
  })
  
  output$text_holt <- renderPrint({
    input$run_holt
    calhw <- input$calhw
    train <- updateData$train
    test  <- updateData$test
    
    if(is.null(isolate(rvmodelo$ms$holt$model))) {
      modelo <- HoltWinters(train)
      updateNumericInput(session, "alpha", value = round(modelo$alpha[[1]], 7))
      updateNumericInput(session, "beta",  value = round(modelo$beta[[1]],  7))
      updateNumericInput(session, "gamma", value = round(modelo$gamma[[1]], 7))
    } else if(isolate(v$checkhw) != calhw) {
      isolate(v$checkhw <- calhw)
      modelo <- calibrar.HW(train, test, isolate(input$paso))
      updateNumericInput(session, "alpha", value = round(modelo$alpha[[1]], 7))
      updateNumericInput(session, "beta",  value = round(modelo$beta[[1]],  7))
      updateNumericInput(session, "gamma", value = round(modelo$gamma[[1]], 7))
    } else {
      alpha  <- isolate(input$alpha)
      beta   <- isolate(input$beta)
      gamma  <- isolate(input$gamma)
      modelo <- HoltWinters(train, alpha, beta, gamma)
    }
    
    pred <- forecast(modelo, h = length(test))$mean
    isolate(rvmodelo$ms$holt$model <- modelo)
    isolate(rvmodelo$ms$holt$pred  <- pred)
    isolate(rvmodelo$ms$holt$error <- tabla.errores(list(pred), test, c("holt")))
    
    modelo
  })
  
  output$table_holt <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$ms$holt$pred, 
                      abs(seriedf[[2]] - rvmodelo$ms$holt$pred))
    colnames(res) <- c(tr('date', lg), "Real", tr('table_m', lg), 
                       tr('diff', lg))
    
    DT::datatable(
      res, selection = 'none', editable = F, rownames = F, 
      options = list(dom = 'frtp', scrollY = "50vh")
    )
  })
  
  output$plot_holt <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$ms$holt$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$fecha <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "fecha")
    colores <- c(input$col_train_holt, input$col_test_holt, input$col_p_holt)
    
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
  
  output$error_holt <- renderUI({
    lg <- updateData$idioma
    
    div(
      style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
      infoBox(tr("mse", lg), rvmodelo$ms$holt$error$MSE, NULL, 
              icon("warning"), "red", 6, fill = T),
      infoBox(tr("rmse", lg), rvmodelo$ms$holt$error$RMSE, NULL, 
              icon("warning"), "yellow", 6, fill = T),
      infoBox(tr("pfa", lg), rvmodelo$ms$holt$error$PFA, NULL, 
              icon("level-up"), "green", 6, fill = T),
      infoBox(tr("ptfa", lg), rvmodelo$ms$holt$error$PTFA, NULL, 
              icon("level-up"), "navy", 6, fill = T)
    )
  })
}
    
## To be copied in the UI
# mod_holtwinters_ui("holtwinters_ui_1")
    
## To be copied in the server
# callModule(mod_holtwinters_server, "holtwinters_ui_1")
 
