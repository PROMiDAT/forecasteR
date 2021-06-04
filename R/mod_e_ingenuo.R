#' e_ingenuo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom forecast forecast snaive
mod_e_ingenuo_ui <- function(id){
  ns <- NS(id)
  
  opc_snaive <- tabsOptions(heights = c(70, 50), tabs.content = list(
    list(
      options.run(ns("run_snaive")), tags$hr(style = "margin-top: 0px;"),
      conditionalPanel(
        condition = "input.BoxSnaive == 'tabPlot'", ns = ns,
        colourpicker::colourInput(
          ns("col_train_snaive"), labelInput("coltrain"), "#5470c6", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_test_snaive"), labelInput("coltest"), "#91cc75", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_p_snaive"), labelInput("colpred"), "#fac858", 
          allowTransparent = T)
      )
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxSnaive"), opciones = opc_snaive, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_snaive")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_snaive'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_snaive"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_snaive"))
      )
    )
  )
}
    
#' e_ingenuo Server Function
#'
#' @noRd 
mod_e_ingenuo_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxSnaive, {
    if(input$BoxSnaive == "tabText") {
      shinyjs::show('run_snaive')
    } else {
      shinyjs::hide('run_snaive')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxSnaive", selected = "tabText")
  })
  
  output$text_snaive <- renderPrint({
    input$run_snaive
    train <- updateData$train
    test  <- updateData$test
    
    modelo <- snaive(train, h = length(test))
    pred   <- modelo$mean
    rvmodelo$ms$snaive$model <- modelo
    rvmodelo$ms$snaive$pred  <- pred
    rvmodelo$ms$snaive$error <- tabla.errores(list(pred), test, c("snaive"))
    
    rvmodelo$ms$snaive$model
  })
  
  output$table_snaive <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$ms$snaive$pred, 
                      abs(seriedf[[2]] - rvmodelo$ms$snaive$pred))
    colnames(res) <- c(tr('date', lg), "Real", tr('table_m', lg), 
                       tr('diff', lg))
    
    DT::datatable(
      res, selection = 'none', editable = F, rownames = F, 
      options = list(dom = 'frtp', scrollY = "50vh")
    )
  })
  
  output$plot_snaive <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$ms$snaive$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$fecha <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "fecha")
    colores <- c(input$col_train_snaive, input$col_test_snaive, input$col_p_snaive)
    
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
  
  output$error_snaive <- renderUI({
    lg <- updateData$idioma
    
    div(
      style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
      infoBox(tr("mse", lg), rvmodelo$ms$snaive$error$MSE, NULL, 
              icon("warning"), "red", 6, fill = T),
      infoBox(tr("rmse", lg), rvmodelo$ms$snaive$error$RMSE, NULL, 
              icon("warning"), "yellow", 6, fill = T),
      infoBox(tr("pfa", lg), rvmodelo$ms$snaive$error$PFA, NULL, 
              icon("level-up"), "green", 6, fill = T),
      infoBox(tr("ptfa", lg), rvmodelo$ms$snaive$error$PTFA, NULL, 
              icon("level-up"), "navy", 6, fill = T)
    )
  })
}
    
## To be copied in the UI
# mod_e_ingenuo_ui("e_ingenuo_ui_1")
    
## To be copied in the server
# callModule(mod_e_ingenuo_server, "e_ingenuo_ui_1")
 
