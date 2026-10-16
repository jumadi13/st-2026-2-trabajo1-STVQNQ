leer_serie <- function(ruta, frecuencia = c("mensual", "trimestral", "diaria")) {
  frecuencia <- match.arg(frecuencia)
  x <- read.csv(ruta, stringsAsFactors = FALSE)
  if (!all(c("fecha", "valor") %in% names(x))) stop("Se requieren columnas fecha y valor")
  x$fecha <- as.Date(x$fecha)
  if (anyNA(x$fecha) || any(!is.finite(x$valor))) stop("Fechas o valores inválidos")
  if (is.unsorted(x$fecha, strictly = TRUE)) stop("Las fechas deben estar ordenadas y ser únicas")
  paso <- switch(frecuencia, diaria = 1, mensual = 30.4375, trimestral = 91.3125)
  if (frecuencia == "diaria" && any(diff(as.numeric(x$fecha)) != 1)) stop("Hay huecos en las fechas")
  if (frecuencia == "mensual" && (any(format(x$fecha, "%d") != "01") || any(diff(as.integer(format(x$fecha, "%Y"))*12 + as.integer(format(x$fecha, "%m"))) != 1))) stop("Secuencia mensual irregular")
  if (frecuencia == "trimestral" && (any(format(x$fecha, "%m-%d") != "01-01" & format(x$fecha, "%m-%d") != "04-01" & format(x$fecha, "%m-%d") != "07-01" & format(x$fecha, "%m-%d") != "10-01") || any(diff(as.integer(format(x$fecha, "%Y"))*4 + (as.integer(format(x$fecha, "%m"))-1)%/%3) != 1))) stop("Secuencia trimestral irregular")
  x
}
