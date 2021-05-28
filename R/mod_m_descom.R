#' m_descom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_m_descom_ui <- function(id){
  ns <- NS(id)
  
  opc_desc <- tabsOptions(heights = c(70, 50), tabs.content = list(
    list(
      options.run(ns("run_desc")), tags$hr(style = "margin-top: 0px;"),
      conditionalPanel(
        condition = "input.BoxDesc == 'tabPlot'", ns = ns,
        colourpicker::colourInput(
          ns("col_train_desc"), labelInput("coltrain"), "#5470c6", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_test_desc"), labelInput("coltest"), "#91cc75", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_p_desc"), labelInput("colpred"), "#fac858", 
          allowTransparent = T)
      )
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxDesc"), opciones = opc_desc, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_desc")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_desc'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_desc"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_desc"))
      )
    )
  )
}
    
#' m_descom Server Function
#'
#' @noRd 
mod_m_descom_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxDesc, {
    if(input$BoxDesc == "tabText") {
      shinyjs::show('run_desc')
    } else {
      shinyjs::hide('run_desc')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxDesc", selected = "tabText")
  })
  
  output$text_desc <- renderPrint({
    input$run_desc
    train <- updateData$train
    test  <- updateData$test
    
    modelo <- stl(train, s.window = "periodic")
    pred   <- forecast(modelo, h = length(test))$mean
    rvmodelo$ms$desc$model <- modelo
    rvmodelo$ms$desc$pred  <- pred
    rvmodelo$ms$desc$error <- tabla.errores(list(pred), test, c("desc"))
    
    rvmodelo$ms$desc$model
  })
  
  output$table_desc <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$ms$desc$pred, 
                      abs(seriedf[[2]] - rvmodelo$ms$desc$pred))
    colnames(res) <- c(tr('date', lg), "Real", tr('table_m', lg), 
                       tr('diff', lg))
    
    DT::datatable(
      res, selection = 'none', editable = F, rownames = F, 
      options = list(dom = 'frtp', scrollY = "50vh")
    )
  })
  
  output$plot_desc <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$ms$desc$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$fecha <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "fecha")
    colores <- ic(input$col_train_desc, input$col_test_desc, input$col_p_desc)
    
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
  
  output$error_desc <- renderUI({
    lg <- updateData$idioma
    
    div(
      style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
      infoBox(tr("mse", lg), rvmodelo$ms$desc$error$MSE, NULL, 
              icon("warning"), "red", 6, fill = T),
      infoBox(tr("rmse", lg), rvmodelo$ms$desc$error$RMSE, NULL, 
              icon("warning"), "yellow", 6, fill = T),
      infoBox(tr("pfa", lg), rvmodelo$ms$desc$error$PFA, NULL, 
              icon("level-up"), "green", 6, fill = T),
      infoBox(tr("ptfa", lg), rvmodelo$ms$desc$error$PTFA, NULL, 
              icon("level-up"), "navy", 6, fill = T)
    )
  })
}
    
## To be copied in the UI
# mod_m_descom_ui("m_descom_ui_1")
    
## To be copied in the server
# callModule(mod_m_descom_server, "m_descom_ui_1")
 
