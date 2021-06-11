#' arima UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats arima
#' @importFrom forecast forecast auto.arima
#' 
mod_arima_ui <- function(id){
  ns <- NS(id)
  
  opc_arima <- div(
    conditionalPanel(
      condition = "input.BoxArima == 'tabText' | input.BoxArima == 'tabPlot' | input.BoxArima == 'tabPeri'", ns = ns,
      tabsOptions(list(icon("gear")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.BoxArima == 'tabText'", ns = ns,
            options.run(ns("run_arima")), tags$hr(style = "margin-top: 0px;"),
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
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            sliderInput(ns("sel_best2"), labelInput("selbest"), 1, 20, 1)
          ),
          conditionalPanel(
            condition = "input.BoxArima == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_arima"), labelInput("coltrain"), "#5470c6", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_test_arima"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_p_arima"), labelInput("colpred"), "#fac858", 
                  allowTransparent = T)
              )
            )
          )
        )
      ))
    )
  )
  
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
    
    tryCatch({
      if(is.null(isolate(rvmodelo$arim$model))) {
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
      isolate(rvmodelo$arim$model <- modelo)
      isolate(rvmodelo$arim$pred  <- pred)
      isolate(rvmodelo$arim$error <- tabla.errores(list(pred), test, c("arim")))
      
      cod <- paste0(
        "modelo.arim <- arima(train, order = c(", modelo$arma[1], ", ", 
        modelo$arma[6], ", ", modelo$arma[2], "), seasonal = list(order = c(",
        modelo$arma[3], ", ", modelo$arma[7], ", ", modelo$arma[4],
        "), period = ", modelo$arma[5], "))\n", 
        "pred.arim   <- modelo.arim$mean\n",
        "error.arim  <- tabla.errores(list(pred.arim), test, 'ARIMA')")
      isolate(updateData$code[['arim']] <- list(docarimm = cod))
      
      modelo
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_acf <- renderEcharts4r({
    train <- updateData$train
    
    tryCatch({
      res <- e_acf(train)
      isolate(updateData$code[['arim']][['docarima']] <- "e_acf(train)")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_pacf <- renderEcharts4r({
    train <- updateData$train
    
    tryCatch({
      res <- e_pacf(train)
      isolate(updateData$code[['arim']][['docarimc']] <- "e_pacf(train)")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_peri <- renderEcharts4r({
    train <- updateData$train
    mejor <- input$sel_best2
    lg    <- updateData$idioma
    
    tryCatch({
      txt <- tr(c('txtbest', 'txtfreq', 'txtperi'), lg)
      res <- e_periods(train, mejor, txt)
      
      isolate(updateData$code[['arim']][['docarimo']] <- paste0(
        "e_periods(train, ", mejor, ", c('", paste(txt, collapse = "','"), "'))"
      ))
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_arima <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$arim$pred, 
                        abs(seriedf[[2]] - rvmodelo$arim$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.arim, abs(s[[2]] - pred.arim))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['arim']][['docarimt']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F,
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_arima <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$arim$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_arima, input$col_test_arima, input$col_p_arima)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.arim')
      isolate(updateData$code[['arim']][['docarimp']] <- code.plots(noms, colors))
      
      opts <- list(
        xAxis = list(
          type = "category", data = format(serie$date, "%Y-%m-%d %H:%M:%S")),
        yAxis = list(show = TRUE),
        series = list(
          list(type = "line", data = serie$train, name = noms[1]),
          list(type = "line", data = serie$test,  name = noms[2]),
          list(type = "line", data = serie$pred,  name = noms[3])
        )
      )
      
      e_charts() %>% e_list(opts) %>% e_legend() %>% e_datazoom() %>% 
        e_tooltip(trigger = 'axis') %>% e_show_loading() %>% e_color(colors)
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$error_arima <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox(tr("mse", lg), rvmodelo$arim$error$MSE, NULL, 
                icon("warning"), "red", 6, fill = T),
        infoBox(tr("rmse", lg), rvmodelo$arim$error$RMSE, NULL, 
                icon("warning"), "yellow", 6, fill = T),
        infoBox(tr("pfa", lg), rvmodelo$arim$error$PFA, NULL, 
                icon("level-up"), "green", 6, fill = T),
        infoBox(tr("ptfa", lg), rvmodelo$arim$error$PTFA, NULL, 
                icon("level-up"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['arim']][['docarime']] <- "error.arim")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_arima_ui("arima_ui_1")
    
## To be copied in the server
# callModule(mod_arima_server, "arima_ui_1")
 
