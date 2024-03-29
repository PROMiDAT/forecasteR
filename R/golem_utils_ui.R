#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#' 
#' @return an HTML list
#' @noRd
#' 
#' @examples
#' list_to_li(c("a","b"))
#'
#' @importFrom htmltools tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$li
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$li
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom htmltools tags tagAppendAttributes tagList
list_to_p <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$p
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$p
    )
    res <- lapply(
      res, 
      function(x) { 
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom htmltools tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL){
  if(is.null(class)){
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list, 
      names(list), 
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list), 
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#' 
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[ attrs[i] ]] <- NULL
  }
  tag
}

#' Hide or display a tag
#' 
#' @param tag the tag
#' 
#' @return a tag
#' @noRd
#' 
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' 
#' @importFrom htmltools tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) && 
    !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;", 
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom htmltools tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) && 
    grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*", 
      "", 
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#' 
#' @param id the id of the element to hide
#' 
#' @noRd
#' 
#' @importFrom htmltools tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#' 
#' @examples
#' with_red_star("Enter your name here")
#' 
#' @importFrom htmltools tags HTML
with_red_star <- function(text) {
  htmltools::tags$span(
    HTML(
      paste0(
        text,
        htmltools::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#' 
#' @examples
#' rep_br(5)
#' 
#' @importFrom htmltools HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' 
#' @importFrom htmltools tags
enurl <- function(url, text){
  tags$a(href = url, text)
}

#' Columns wrappers
#' 
#' These are convenient wrappers around 
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#' 
#' @noRd
#' 
#' @importFrom shiny column
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_11 <- function(...){
  column(11, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_9 <- function(...){
  column(9, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_7 <- function(...){
  column(7, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}

#' @importFrom shiny column
col_5 <- function(...){
  column(5, ...)
}

#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}

#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}

#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}

#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}

# UNCOMMENT AND USE 
# 
# usethis::use_package("markdown")
# usethis::use_package("rmarkdown")
#   
# To use this part of the UI
#   
#' #' Include Content From a File
#' #' 
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #' 
#' #' @rdname includeRMarkdown
#' #' @export
#' #' 
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom htmltools HTML
#' includeRMarkdown <- function(path){
#'   
#'   md <- tempfile(fileext = '.md')
#'   
#'   on.exit(unlink(md),add = TRUE)
#'   
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'   
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'   
#'   Encoding(html) <- "UTF-8"
#'   
#'   return(HTML(html))
#' }

#' Returns HTML for language
#'
#' @return HTML
#' @noRd
#' 
#' @examples
#' menu.idioma()
#' 
menu.idioma <- function() {
  tags$li(
    class = "nodisabled treeview",
    tags$a(
      href = "#shiny-tab-tabdioma",
      tags$i(class="fa fa-language"),
      labelInput("idioma"),
      tags$i(class="fa fa-angle-left pull-right")),
    tags$ul(
      class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
      radioButtons('idioma', labelInput("selidioma"),
                   choiceNames = c(tr('espanol', 'es'), 'English'),
                   choiceValues = c("es", "en")),
      tags$br()))
}

options.run <- function(runid = NULL) {
  tags$div(
    style = "display: inline-block; width: 100%", 
    shiny::h4(labelInput("opciones"), 
              style = "float: left;margin-bottom: 0px;margin-top: 0px;"),
    if(!is.null(runid)) {
      tags$button(
        id = runid, type = "button", class = "run-button action-button", 
        icon("play"), tags$a(labelInput("ejecutar"), style = "color:white"))
    }
  )
}

tabBoxPrmdt <- function (..., id = NULL, title = NULL, opciones = NULL) {
  content <- shiny::tabsetPanel(..., id = id, selected = NULL)
  content$attribs$class <- "nav-tabs-custom"
  
  if (!is.null(title)) {
    content$children[[1]] <- htmltools::tagAppendChild(
      content$children[[1]], tags$li(class = "header pull-right", title))
  }
  if (!is.null(opciones)) {
    pos <- length(content$children[[2]]$children) + 1
    content$children[[2]]$children[[pos]] <- opciones
  }
  
  content
}

tabsOptions <- function(
  botones = list(icon("gear"), icon("code")), widths = c(50, 100),
  heights = c(100, 50), tabs.content = list("", "")
) {
  res <- ""
  codeButtons <- ""
  cant <- length(botones)
  if(cant == 1) {widgets <- c("center")}
  if(cant == 2) {widgets <- c("left", "right")}
  if(cant == 3) {widgets <- c("left", "center", "right")}
  if(cant == 4) {widgets <- c("left", "centerleft", "centeright", "right")}
  if(cant == 5) {
    widgets <- c("left", "centerleft", "center", "centeright", "right")}
  for (i in 1:cant) {
    res <- paste0(
      res, tags$div(
        class = paste0("box-option box-option-", widgets[i]),
        style = paste0("width:", widths[i], "%;height:", heights[i], "%;"),
        tabs.content[[i]]), "\n")
    codeButtons <- paste0(
      codeButtons, "<button style='width:", 100/cant, "%' data-widget='", 
      widgets[i], "'>", botones[[i]], "</button>\n")
  }
  res <- paste0(
    res, tags$div(
      class = "btn-options", style = "position:relative;", 
      width = "100%", HTML(codeButtons))
  )
  return(tags$div(HTML(res)))
}

tabVerticalBox <- function (..., id = NULL, selected = NULL, width = 6, height = NULL, side = c("left", "right")) {
  side <- match.arg(side)
  content <- shiny::tabsetPanel(..., id = id, selected = selected)
  content$attribs$class <- "nav-tabs-vertical"
  content$children[[1]]$attribs$class <- "nav vertical-tabs shiny-tab-input"
  if (!is.null(height)) {
    content$children[[1]] <- tagAppendAttributes(
      content$children[[1]], style = paste0("height: ", validateCssUnit(height)))
    content$children[[2]] <- tagAppendAttributes(
      content$children[[2]], style = paste0("height: ", validateCssUnit(height)))
  }
  if (side == "right") {
    content$children[[1]] <- tagAppendAttributes(content$children[[1]], 
                                                 class = "pull-right")
  }
  
  div(class = paste0("col-sm-", width), content)
}

numericDisabled<- function (inputId, label, value, min = NA, max = NA, step = NA, width = NULL) {
  content <- numericInput(inputId, label, value, min, max, step, width)
  content$children[[2]]$attribs[["disabled"]] <- "disabled"
  return(content)
}
