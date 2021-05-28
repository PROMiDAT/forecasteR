#' arima UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_arima_ui <- function(id){
  ns <- NS(id)
  
  opc_arima <- tabsOptions(heights = c(100, 50), tabs.content = list(
    list(
      options.run(ns("run_arima")), tags$hr(style = "margin-top: 0px;"),
      conditionalPanel(
        condition = "input.BoxArima == 'tabText'", ns = ns,
        fluidRow(
          col_4(numericInput(ns('p'), 'p', 0, 0, step = 0.5)),
          col_4(numericInput(ns('d'), 'd', 0, 0, step = 0.5)),
          col_4(numericInput(ns('q'), 'q', 0, 0, step = 0.5))
        ),
        fluidRow(
          col_4(numericInput(ns('P'), 'P', 0, 0, step = 0.5)),
          col_4(numericInput(ns('D'), 'D', 0, 0, step = 0.5)),
          col_4(numericInput(ns('Q'), 'Q', 0, 0, step = 0.5))
        ),
        numericInput(ns('periodo'), labelInput('selperi'), 0, 0, step = 0.5),
        tags$hr(style = "margin-top: 0px;"), h4(labelInput('calip')),
        tags$hr(style = "margin-top: 0px;"),
        sliderInput(ns('ar'), labelInput('lar'), 1, 10, 2, 1),
        sliderInput(ns('es'), labelInput('les'), 1, 10, 1, 1),
        actionButton(ns('calarima'), labelInput('cali'), width = '100%')
      ),
      conditionalPanel(
        condition = "input.BoxArima == 'tabPeri'", ns = ns,
        sliderInput(ns("sel_best2"), labelInput("selbest"), 1, 20, 1)
      ),
      conditionalPanel(
        condition = "input.BoxArima == 'tabPlot'", ns = ns,
        colourpicker::colourInput(
          ns("col_train_arima"), labelInput("coltrain"), "#5470c6", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_test_arima"), labelInput("coltest"), "#91cc75", 
          allowTransparent = T),
        colourpicker::colourInput(
          ns("col_p_arima"), labelInput("colpred"), "#fac858", 
          allowTransparent = T)
      )
    ),
    list(#codigo.monokai(ns("fieldCodeCor"),  height = "30vh"))
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxArima"), opciones = opc_arima, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_arima")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("corre"), value = "tabCorre",
        echarts4rOutput(ns("plot_acf"),  height = "35vh"),
        echarts4rOutput(ns("plot_pacf"), height = "35vh")
      ),
      tabPanel(
        title = labelInput("peri"), value = "tabPeri",
        echarts4rOutput(ns("plot_peri"),  height = "70vh")
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_arima'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_arima"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_arima"))
      )
    )
  )
}
    
#' arima Server Function
#'
#' @noRd 
mod_arima_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  v <- rv(checkarima = 0)
  
  observeEvent(input$BoxArima, {
    if(input$BoxArima == "tabText") {
      shinyjs::show('run_arima')
    } else {
      shinyjs::hide('run_arima')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxArima", selected = "tabText")
  })
  
  output$text_arima <- renderPrint({
    input$run_arima
    calarima <- input$calarima
    train <- updateData$train
    test  <- updateData$test
    
    if(is.null(isolate(rvmodelo$ms$arima$model))) {
      modelo <- auto.arima(train)
      updateNumericInput(session, "p", value = modelo$arma[1])
      updateNumericInput(session, "d", value = modelo$arma[6])
      updateNumericInput(session, "q", value = modelo$arma[2])
      updateNumericInput(session, "P", value = modelo$arma[3])
      updateNumericInput(session, "D", value = modelo$arma[7])
      updateNumericInput(session, "Q", value = modelo$arma[4])
      updateNumericInput(session, "periodo", value = modelo$arma[5])
    } else if(isolate(v$checkarima) != calarima) {
      isolate(v$checkarima <- calarima)
      ar     <- isolate(input$ar)
      es     <- isolate(input$es)
      modelo <- calibrar.arima(train, test, isolate(input$periodo), 0:ar, 0:es)
      updateNumericInput(session, "p", value = modelo$arma[1])
      updateNumericInput(session, "d", value = modelo$arma[6])
      updateNumericInput(session, "q", value = modelo$arma[2])
      updateNumericInput(session, "P", value = modelo$arma[3])
      updateNumericInput(session, "D", value = modelo$arma[7])
      updateNumericInput(session, "Q", value = modelo$arma[4])
    } else {
      p <- isolate(input$p)
      d <- isolate(input$d)
      q <- isolate(input$q)
      P <- isolate(input$P)
      D <- isolate(input$D)
      Q <- isolate(input$Q)
      periodo <- isolate(input$periodo)
      modelo  <- arima(train, order = c(p, d, q), 
                       seasonal = list(order = c(P, D, Q), period = periodo))
    }
    
    pred   <- forecast(modelo, h = length(test))$mean
    isolate(rvmodelo$ms$arima$model <- modelo)
    isolate(rvmodelo$ms$arima$pred  <- pred)
    isolate(rvmodelo$ms$arima$error <- tabla.errores(list(pred), test, c("arima")))
    
    modelo
  })
  
  output$plot_acf <- renderEcharts4r({
    train <- updateData$train
    
    res  <- acf(train, plot = F)
    clim <- qnorm((1 + 0.95)/2)/sqrt(res$n.used)
    res <- data.frame(Lag = res$lag[, , 1], ACF = res$acf[, , 1])
    lim  <- round(min(c(res$ACF, -clim)) - 0.06, 1)
    
    res %>% e_charts(Lag) %>% e_bar(ACF, barMaxWidth = 3) %>% 
      e_mark_line(data = list(yAxis = clim)) %>% 
      e_mark_line(data = list(yAxis = -clim)) %>% 
      e_legend(show = F) %>% e_y_axis(min = lim, name = 'AFC') %>%
      e_x_axis(name = 'Lag') %>% e_show_loading()
  })
  
  output$plot_pacf <- renderEcharts4r({
    train <- updateData$train
    
    res  <- pacf(train, plot = F)
    clim <- qnorm((1 + 0.95)/2)/sqrt(res$n.used)
    res <- data.frame(Lag = res$lag[, , 1], ACF = res$acf[, , 1])
    lim  <- round(min(c(res$ACF, -clim)) - 0.06, 1)
    
    res %>% e_charts(Lag) %>% e_bar(ACF, barMaxWidth = 3) %>% 
      e_mark_line(data = list(yAxis = clim)) %>% 
      e_mark_line(data = list(yAxis = -clim)) %>% 
      e_legend(show = F) %>% e_y_axis(min = lim, name = 'PAFC') %>%
      e_x_axis(name = 'Lag') %>% e_show_loading()
  })
  
  output$plot_peri <- renderEcharts4r({
    train <- updateData$train
    mejor <- input$sel_best2
    plot_periods(train, mejor)
  })
  
  output$table_arima <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$ms$arima$pred, 
                      abs(seriedf[[2]] - rvmodelo$ms$arima$pred))
    colnames(res) <- c(tr('date', lg), "Real", tr('table_m', lg), 
                       tr('diff', lg))
    
    DT::datatable(
      res, selection = 'none', editable = F, rownames = F, 
      options = list(dom = 'frtp', scrollY = "50vh")
    )
  })
  
  output$plot_arima <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$ms$arima$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$fecha <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "fecha")
    colores <- c(input$col_train_arima, input$col_test_arima, input$col_p_arima)
    
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
  
  output$error_arima <- renderUI({
    lg <- updateData$idioma
    
    div(
      style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
      infoBox(tr("mse", lg), rvmodelo$ms$arima$error$MSE, NULL, 
              icon("warning"), "red", 6, fill = T),
      infoBox(tr("rmse", lg), rvmodelo$ms$arima$error$RMSE, NULL, 
              icon("warning"), "yellow", 6, fill = T),
      infoBox(tr("pfa", lg), rvmodelo$ms$arima$error$PFA, NULL, 
              icon("level-up"), "green", 6, fill = T),
      infoBox(tr("ptfa", lg), rvmodelo$ms$arima$error$PTFA, NULL, 
              icon("level-up"), "navy", 6, fill = T)
    )
  })
}
    
## To be copied in the UI
# mod_arima_ui("arima_ui_1")
    
## To be copied in the server
# callModule(mod_arima_server, "arima_ui_1")
 
