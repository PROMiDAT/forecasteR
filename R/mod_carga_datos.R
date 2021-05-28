#' carga_datos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_carga_datos_ui <- function(id) {
  ns <- NS(id)
  btn_s <- "width: 100%;float: right;background-color: #3c8dbc;color: white;"
  btn_s_n <- "width: 100%;float: right;background-color: #3c8dbc;color: white;display: none;"
  
  tagList(
    div(
      id = ns("cargadf"),
      div(col_10(
        box(
          title = labelInput("data"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE,
          flowLayout(
            checkboxInput(ns('header'), labelInput("header"), value = T),
            radioButtons(
              ns('sep'), labelInput("separador"), inline = T,
              choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
            ),
            radioButtons(ns('dec'), labelInput("separadordec"), c(',', '.'),
                         inline = T),
            fileInput(
              ns('archivo'), labelInput("cargarchivo"), width = "100%",
              placeholder = "", buttonLabel = labelInput("subir"),
              accept = c('text/csv', '.csv', '.txt'))
          ), hr(),
          actionButton(ns("loadButton"), labelInput("cargar"), width = "100%"),
          footer = div(
            withLoader(DT::dataTableOutput(ns('tabladatos')), 
                       type = "html", loader = "loader4"))
        )),
        col_2(actionButton(ns("btn_next"), labelInput("sig"), style = btn_s_n))
      )
    ),
    div(
      id = ns("cargatsdf"), style = "display: none;",
      div(
        col_2(actionButton(ns("btn_prev"), labelInput("ant"), style = btn_s)),
        col_8(box(
          title = labelInput("cargar"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE,
          fluidRow(
            col_4(
              h3(labelInput('data')),
              selectInput(ns("sel_valor"), labelInput('selvalor'), "")
            ),
            col_8(
              h3(labelInput('date')),
              radioButtons(
                ns('colFecha'), NULL, inline = T, 
                choiceNames = list(labelInput('sel'), labelInput('cre')), 
                choiceValues = c('colum', 'nuevo')
              ),
              conditionalPanel(
                condition = "input.colFecha == 'colum'", ns = ns,
                selectInput(ns("sel_fecha"), labelInput('selfecha'), "")
              ),
              conditionalPanel(
                condition = "input.colFecha == 'nuevo'", ns = ns,
                selectInput(ns("tipofecha"), labelInput('seltipo'), NULL),
                uiOutput(ns("uifechas"))
              )
            ), 
            col_12(hr(), actionButton(ns("tsdfButton"), labelInput("cargar"), 
                                      width = "100%"))
          ),
          footer = div(
            withLoader(DT::dataTableOutput(ns('seriedatos')), 
                       type = "html", loader = "loader4"))
        )),
        col_2(actionButton(ns("btn_next2"), labelInput("sig"), style = btn_s_n))
      )
    ),
    div(
      id = ns("cargats"), style = "display: none;",
      div(
        col_2(actionButton(ns("btn_prev2"), labelInput("ant"), style = btn_s)),
        col_10(box(
          title = labelInput("cargar"), status = "primary", 
          width = 12, solidHeader = T, collapsible = T,
          fluidRow(
            col_4(selectInput(ns("sel_patron"), labelInput('selpatron'), "")),
            col_8(sliderInput(ns("n_tt"), label = div(
              div(style = 'float: left; color: #428bca;', labelInput('train')),
              div(style = 'float: right; color: #91cc75;', labelInput('test'))),
              5, 95, 80, 5))), hr(),
            actionButton(ns("tsButton"), labelInput("cargar"), width = "100%"),
          footer = echarts4rOutput(ns('plot_ts'), height = "60vh")
        ))
      )
    )
  )
}
    
#' carga_datos Server Function
#' @keywords internal
mod_carga_datos_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  updateDate <- rv(ini = NULL, fin = NULL)
  
  # Idioma
  observeEvent(updateData$idioma, {
    lg <- updateData$idioma
    fechas <- list("years", "months", "days", "hours", "min", "sec")
    names(fechas) <- c(tr("anual", lg), tr("mes", lg), tr("dia", lg), 
                       tr("hora", lg), tr("minuto", lg), tr("segundo", lg))
    updateSelectInput("tipofecha", session = session, choices = fechas)
  })
  
  #' Hide/Show Menu
  observeEvent(input$btn_next, {
    hide(id = "cargadf", anim = T, animType = "slide")
    hide(id = "cargats", anim = T, animType = "slide")
    show(id = "cargatsdf", anim = T, animType = "slide")
  })
  observeEvent(input$btn_prev, {
    show(id = "cargadf", anim = T, animType = "slide")
    hide(id = "cargats", anim = T, animType = "slide")
    hide(id = "cargatsdf", anim = T, animType = "slide")
  })
  observeEvent(input$btn_prev2, {
    hide(id = "cargadf", anim = T, animType = "slide")
    hide(id = "cargats", anim = T, animType = "slide")
    show(id = "cargatsdf", anim = T, animType = "slide")
  })
  observeEvent(input$btn_next2, {
    hide(id = "cargadf", anim = T, animType = "slide")
    show(id = "cargats", anim = T, animType = "slide")
    hide(id = "cargatsdf", anim = T, animType = "slide")
  })
  
  ############################# Carga de datos ################################
  
  #' Función del botón loadButton
  observeEvent(input$loadButton, {
    rvmodelo$ms <- list(prom = NULL, inge = NULL, eing = NULL, drif = NULL,
                        deco = NULL, holt = NULL, arim = NULL)
    updateData$datos   <- NULL
    updateData$seriedf <- NULL
    updateData$seriets <- NULL
    updateData$train   <- NULL
    updateData$test    <- NULL
    updateData$ts_type <- NULL
    
    ruta       <- isolate(input$archivo)
    sep        <- isolate(input$sep)
    dec        <- isolate(input$dec)
    encabezado <- isolate(input$header)
    tryCatch({
      #codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      #updateAceEditor(session, "fieldCodeData", value = codigo)
      
      updateData$datos <- carga.datos(ruta$datapath, sep, dec, encabezado)
      if(ncol(var.numericas(updateData$datos)) <= 0) {
        updateData$datos <- NULL
        showNotification("ERROR 00020: Check Separators", type = "error")
      }
    }, error = function(e) {
      updateData$datos <- NULL
      showNotification(paste0("ERROR 00010: ", e), type = "error")
    })
  })
  
  #' Actualizar tabla al cargar los datos
  output$tabladatos <- DT::renderDataTable({
    datos  <- updateData$datos
    nombre <- str_remove(isolate(input$archivo$name), '\\..[^\\.]*$')
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        extensions = 'Buttons', options = list(
          dom = 'Bfrtip', scrollY = "30vh", 
          buttons = list(list(extend = 'csv', filename = nombre, 
                              text = '<i class="fa fa-download"></i>'))),
      )
    }, error = function(e) {
      showNotification(paste0("ERROR 00030: ", e), type = "error")
      return(NULL)
    })
  }, server = F)
  
  #' Actualiza opciones al cargar tabla de datos
  observeEvent(updateData$datos, {
    datos     <- updateData$datos
    numericos <- var.numericas(datos)
    
    if(is.null(datos)) {
      hide(id = "btn_next", anim = T, animType = "fade")
    } else {
      show(id = "btn_next", anim = T, animType = "fade")
    }
    
    updateSelectInput(session, "sel_valor", choices = colnames(numericos))
    updateSelectInput(session, "sel_fecha", choices = colnames(datos))
  }, ignoreNULL = FALSE)
  
  ############################# Carga Serie DF ################################
  
  #' Generar input de fechas
  output$uifechas <- renderUI({
    if(input$tipofecha == "years") {
      f <- 'YYYY-01-01 00:00:00'
    } else if(input$tipofecha == "months") {
      f <- 'YYYY-MM-01 00:00:00'
    } else if(input$tipofecha == "days") {
      f <- 'YYYY-MM-DD 00:00:00'
    } else {
      f <- 'YYYY-MM-DD HH:mm:SS'
    }
    
    texto <- tr("hasta", updateData$idioma)
    
    fluidRow(
      col_5(datetimeInput(ns("startdate"), f)),
      col_2(h4(texto, style = "text-align: center;")),
      col_5(datetimeInput(ns("enddate"), f))
    )
  })
  
  #' Actualizar fecha final
  observeEvent(input$startdate, {
    tryCatch({
      tipofecha <- isolate(input$tipofecha)
      n <- nrow(isolate(updateData$datos)) - 1
      ini <- ymd_hms(input$startdate)
      if(tipofecha == "months") {
        fin <- ini + months(n)
      } else {
        fin <- ini + duration(n, units = tipofecha)
      }
      
      runjs(paste0(
        "$('#carga_datos_ui_1-enddate').find('input').val('", fin, "');"))
      
      updateDate$ini <- ini
      updateDate$fin <- fin
    }, error = function(e) {})
  })
  
  #' Actualizar fecha inicial
  observeEvent(input$enddate, {
    tryCatch({
      tipofecha <- isolate(input$tipofecha)
      n <- nrow(isolate(updateData$datos)) - 1
      fin <- ymd_hms(input$enddate)
      if(tipofecha == "months") {
        ini <- fin - months(n)
      } else {
        ini <- fin - duration(n, units = tipofecha)
      }
      
      runjs(paste0(
        "$('#carga_datos_ui_1-startdate').find('input').val('", ini, "');"))
      
      updateDate$ini <- ini
      updateDate$fin <- fin
    }, error = function(e) {})
  })
  
  #' Función del botón tsdfButton
  observeEvent(input$tsdfButton, {
    rvmodelo$ms <- list(prom = NULL, inge = NULL, eing = NULL, drif = NULL,
                        deco = NULL, holt = NULL, arim = NULL)
    
    updateData$seriedf <- NULL
    updateData$seriets <- NULL
    updateData$train   <- NULL
    updateData$test    <- NULL
    updateData$ts_type <- NULL
    
    tryCatch({
      #codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      #updateAceEditor(session, "fieldCodeData", value = codigo)
      datos <- isolate(updateData$datos)
      
      if(input$colFecha == "nuevo") {
        ini <- isolate(updateDate$ini)
        fin <- isolate(updateDate$fin)
        fechas <- seq(ini, fin, by = isolate(input$tipofecha))
        
        updateData$seriedf <- data.frame(
          fechas = fechas, valor = datos[[input$sel_valor]])
        updateData$ts_type <- isolate(input$tipofecha)
      } else {
        fechas <- text_toDate(datos[[input$sel_fecha]])
        
        updateData$seriedf <- data.frame(
          fechas = fechas[[1]], valor = datos[[input$sel_valor]])
        updateData$ts_type <- fechas[[2]]
      }
    }, error = function(e) {
      updateData$seriedf <- NULL
      updateData$ts_type <- NULL
      showNotification(paste0("ERROR 00040: ", e), type = "error")
    })
  })
  
  #' Actualizar la tabla al cargar la serie de datos df
  output$seriedatos <- DT::renderDataTable({
    datos  <- updateData$seriedf
    idioma <- isolate(updateData$idioma)
    nombre <- paste0(str_remove(isolate(input$archivo$name), '\\..[^\\.]*$'),
                     "_ts")
    
    tryCatch({
      if(!is.null(datos)) {
        datos$fechas <- as.character(datos$fechas)
        colnames(datos) <- c(tr("fecha", idioma), tr("valor", idioma))
      }
      DT::datatable(
        datos, selection = 'none', rownames = F, extensions = 'Buttons',
        options = list(dom = 'Bfrtip', scrollY = "30vh", buttons = list(list(
          extend = 'csv', filename = nombre, 
          text = '<i class="fa fa-download"></i>')))
      )
    }, error = function(e) {
      showNotification(paste0("ERROR 00050: ", e), type = "error")
      return(NULL)
    })
  }, server = F)
  
  #' Actualiza las opciones al cargar tabla de datos
  observeEvent(updateData$seriedf, {
    serie <- updateData$seriedf
    
    if(is.null(serie)) {
      hide(id = "btn_next2", anim = T, animType = "fade")
    } else {
      show(id = "btn_next2", anim = T, animType = "fade")
    }
  }, ignoreNULL = FALSE)
  
  ############################# Carga Serie ts ################################
  
  # Actualizar al obtener la serie o cambiar el idioma
  observeEvent(c(updateData$ts_type, updateData$idioma), {
    lg <- updateData$idioma
    tipo <- updateData$ts_type
    
    if(!is.null(tipo)) {
      if(tipo == "years") {
        fechas <- list(1)
        names(fechas) <- c(tr("anual", lg))
      } else if(tipo == "months") {
        fechas <- list(12)
        names(fechas) <- c(tr("anual", lg))
      } else if(tipo == "days") {
        fechas <- list(365)
        names(fechas) <- c(tr("anual", lg))
      } else if(tipo == "hours") {
        fechas <- list(24, 8760)
        names(fechas) <- c(tr("dia", lg), tr("anual", lg))
      } else if(tipo == "min") {
        fechas <- list(60, 1440, 525600)
        names(fechas) <- c(tr("hora", lg), tr("dia", lg), tr("anual", lg))
      } else if(tipo == "sec") {
        fechas <- list(60, 3600, 86400, 31536000)
        names(fechas) <- c(tr("minuto", lg), tr("hora", lg), tr("dia", lg), 
                           tr("anual", lg))
      }
      
      updateSelectInput("sel_patron", session = session, choices = fechas)
    }
  })
  
  observeEvent(input$tsButton, {
    rvmodelo$ms <- list(prom = NULL, inge = NULL, eing = NULL, drif = NULL,
                        deco = NULL, holt = NULL, arim = NULL)
    
    updateData$seriets <- NULL
    updateData$train   <- NULL
    updateData$test    <- NULL
    
    tryCatch({
      #codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      #updateAceEditor(session, "fieldCodeData", value = codigo)
      datos   <- isolate(updateData$seriedf)
      tipo    <- isolate(updateData$ts_type)
      f       <- as.numeric(isolate(input$sel_patron))
      n_tt    <- isolate(input$n_tt)
      n_train <- round(nrow(datos) * (n_tt/100))
      n_test  <- nrow(datos) - n_train
      s       <- get_start(datos[[1]][1], tipo, f)
      
      serie <- ts(datos[[2]], start = c(1, s), frequency = f)
      updateData$seriets <- serie
      updateData$train   <- head(serie, n_train)
      updateData$test    <- tail(serie, n_test)
    }, error = function(e) {
      updateData$seriets <- NULL
      updateData$train   <- NULL
      updateData$test    <- NULL
      showNotification(paste0("ERROR 00060: ", e), type = "error")
    })
  })
  
  #' Grafico serie de tiempo
  output$plot_ts <- renderEcharts4r({
    train <- updateData$train
    test  <- updateData$test
    names <- c(tr('train', updateData$idioma), tr('test', updateData$idioma))
    if(is.null(train) | is.null(test)) {
      return(NULL)
    }
    
    serie <- data.frame(ts.union(train, test))
    serie$fecha <- isolate(updateData$seriedf)[[1]]
    
    tryCatch({
      serie %>% e_charts(x = fecha) %>% 
        e_line(serie = train, name = names[1]) %>%
        e_line(serie = test,  name = names[2]) %>% 
        e_datazoom() %>% e_tooltip(trigger = "axis") %>% e_show_loading()
    }, error = function(e) {
      showNotification(paste0("ERROR 00070: ", e), type = "error")
      return(NULL)
    })
  })
}

## To be copied in the UI
# mod_carga_datos_ui("carga_datos_ui_1")
    
## To be copied in the server
# callModule(mod_carga_datos_server, "carga_datos_ui_1")
 
