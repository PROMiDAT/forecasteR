datetimeInput <- function(id, f = 'YYYY-01-01') {
  tagList(
    div(
      id = id, class = "input-group-date input-group date",
      tags$input(type = "text", class = "form-control"),
      tags$span(class = "input-group-addon", tags$i(class = "fa fa-calendar"))
    ),
    tags$script(HTML(paste0("$('#", id, "').datetimepicker({format: '", f, "'});")))
  )
}

infoBoxPROMiDAT <- function(titulo, valor, icono) {
  tags$div(
    class = "info-box bg-promidat",
    tags$span(class = "info-box-icon", icono),
    tags$div(class="info-box-content", 
             tags$span(class = "info-box-text", titulo),
             tags$span(class = "info-box-number", valor)
    )
  )
}

labelInput <- function(inputId, value = ""){
  tags$span(`data-id` = inputId, value)
}

updateLabelInput <- function (session, labelid, value = NULL) {
  message <- dropNulls(list(labelid = labelid))
  if(length(labelid) == 1) {
    labelid <- list(labelid)
  }
  ifelse(
    is.null(value), sentvalue <- labelid,
    ifelse(length(value) == 1, sentvalue <- list(value),
           sentvalue <- value))
  session$sendCustomMessage(
    type = 'updateLabel',
    message = list(ids = labelid, values = sentvalue))
}