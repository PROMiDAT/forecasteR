#' Filter numeric variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export var.numericas
#' @examples
#' var.numericas(iris)
#' 
var.numericas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
}

#' Filter category variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export var.categoricas
#' @examples
#' var.categoricas(iris)
#' 
var.categoricas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
}

#' Convert character to dates
#'
#' @param f a vector of character.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return vector
#' @import stringr
#' @export text_toDate
#' @examples
#' text_toDate(iris)
#' 
text_toDate <- function(f) {
  e <- list(y = NA, ms = NA, d = NA, h = NA, m = NA, s = NA)
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
             "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  meses.abv <- str_extract(meses, "\\w{3}")
  months <- c("january", "february", "march", "april", "may", "june", "july", 
              "august", "september", "october", "november", "december")
  months.abb <- str_extract(months, "\\w{3}")
  f <- str_to_lower(f)
  
  e[['y']] <- str_extract(f, "\\d{4}")
  f <- str_remove(f, "\\d{4}")
  
  f  <- str_replace(f, "setiembre", "septiembre")
  f  <- str_replace(f, "set", "sep")
  e[['ms']] <- str_extract(f, paste(meses, collapse = "|"))
  if(any(is.na(e[['ms']]))) {
    e[['ms']] <- str_extract(f, paste(meses.abv, collapse = "|"))
    meses <- meses.abv
  }
  if(!any(is.na(e[['ms']]))) {
    e[['ms']] <- sapply(e[['ms']], function(m) which(m == meses))
  }
  
  aux <- str_extract(f, "\\d{2}")
  while(!any(is.na(aux))) {
    aux <- as.numeric(aux)
    if(max(aux) <= 12 & any(is.na(e[['ms']]))) {
      e[['ms']] <- aux
    } else if(max(aux) <= 23) {
      e[['h']] <- aux
    } else if(max(aux) <= 31) {
      e[['d']] <- aux
    } else if(max(aux) <= 59 & any(is.na(e[['m']]))) {
      e[['m']] <- aux
    } else if(max(aux) <= 59) {
      e[['s']] <- aux
    } else if(any(is.na(e[['y']]))) {
      e[['y']] <- aux
    }
    
    f <- str_remove(f, "\\d{2}")
    aux <- str_extract(f, "\\d{2}")
  }
  
  tipo <- 'years'
  ifelse(any(is.na(e[["y"]])),  e[["y"]] <- 2020, tipo <- 'years')
  ifelse(any(is.na(e[["ms"]])), e[["ms"]] <- 1, tipo <- 'months')
  ifelse(any(is.na(e[["d"]])),  e[["d"]] <- 1, tipo <- 'days')
  ifelse(any(is.na(e[["h"]])),  e[["h"]] <- 0, tipo <- 'hours')
  ifelse(any(is.na(e[["m"]])),  e[["m"]] <- 0, tipo <- 'min')
  ifelse(any(is.na(e[["s"]])),  e[["s"]] <- 0, tipo <- 'sec')
  
  res <- paste0(e[["y"]], "-", e[["ms"]], "-", e[["d"]], " ", 
                e[["h"]], ":", e[["m"]], ":", e[["s"]])
  res <- list(as.POSIXct(res, tz = "UTC"), tipo)
  
  return(res)
}

get_start <- function(ini, tipo_f, patron) {
  if(patron == 24) {
    return(hour(ini) + 1)
  } else if(patron == 60 & tipo_f == "min") {
    return(minute(ini) + 1)
  } else if(patron == 1440) {
    return(hour(ini) * 60 + minute(ini) + 1)
  } else if(patron == 60) {
    return(second(ini) + 1)
  } else if(patron == 3600) {
    return(minute(ini) * 60 + second(ini) + 1)
  } else if(patron == 86400) {
    return(hour(ini) * 3600 + minute(ini) * 60 + second(ini) + 1)
  } else {
    res <- 1
    fecha <- ymd_hms(paste0(year(ini), "-01-01 00:00:00"))
    
    while(ini != fecha) {
      res <- res + 1
      fecha <- seq(fecha, length = 2, by = tipo_f)[2]
    }
    return(res)
  }
}

carga.datos <- function(ruta = NULL, sep = ";", dec = ",", encabezado = T) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  return(read.table(ruta, header = encabezado, sep = sep, dec = dec))
}

############################### Generar Código ################################
# code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";",
#                        sep.decimal = ",", encabezado = T, incluir.NA = F) {
#   res <- paste0(
#     "datos <- read.table(stringsAsFactors = T, '", ruta, "', header=", encabezado, 
#     ", sep='", separador, "', dec = '", sep.decimal, "'", 
#     ifelse(nombre.filas, ", row.names = 1", ""), ")")
#   res <- paste0(res, "\n", code.NA(incluir.NA))
#   return(res)
# }
# 
# code.NA <- function(deleteNA = T) {
#   res <- ifelse(
#     deleteNA, "datos <- na.omit(datos)\n",
#     paste0(
#       "Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
#       "for (var in colnames(datos)) {\n",
#       "  if(any(is.na(datos[, var]))){\n",
#       "    if(class(datos[, var]) %in% c('numeric', 'integer')) {\n",
#       "      datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)\n",
#       "    } else {\n",
#       "      datos[, var][is.na(datos[, var])] <- Mode(datos[, var])\n",
#       "    }\n  }\n}"))
#   return(res)
# }
# 
# code.trans <- function(var, nuevo.tipo) {
#   if(nuevo.tipo == "categorico"){
#     return(paste0(
#       "datos[['", var, "']] <- as.factor(datos[['", var, "']])\n"))
#   } else if(nuevo.tipo == "numerico") {
#     return(paste0(
#       "datos[['", var, "']] <- as.numeric(sub(',', '.', datos[['",
#       var, "']], fixed = TRUE))\n"))
#   } else {
#     return(paste0(
#       "datos <- datos.disyuntivos(datos, '", var,"')\n", 
#       "datos[['", var, "']] <- NULL\n"))
#   }
# }