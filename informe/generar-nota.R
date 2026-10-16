
# Regenera la nota ejecutiva usando las salidas actuales de analisis/.
# Ejecutar desde la raíz del repositorio:
# source("informe/generar-nota.R")

if (!file.exists("analisis/evaluacion-origen-movil.csv") ||
    !file.exists("analisis/resumen-descriptivo.csv")) {
  stop(
    "No se encuentran los CSV de resultados. ",
    "Ejecute primero analisis/correr-todo.R."
  )
}

# ------------------------------------------------------------
# 1. Cargar y validar los resultados
# ------------------------------------------------------------

ev <- read.csv(
  "analisis/evaluacion-origen-movil.csv",
  stringsAsFactors = FALSE
)

desc <- read.csv(
  "analisis/resumen-descriptivo.csv",
  stringsAsFactors = FALSE
)

columnas_ev <- c("serie", "horizonte", "metodo", "RMSE")
columnas_desc <- c("serie", "media")

if (!all(columnas_ev %in% names(ev))) {
  stop(
    "El CSV de evaluación debe contener las columnas: ",
    paste(columnas_ev, collapse = ", "),
    ". Revise los nombres reales de las columnas."
  )
}

if (!all(columnas_desc %in% names(desc))) {
  stop(
    "El CSV descriptivo debe contener las columnas: ",
    paste(columnas_desc, collapse = ", ")
  )
}

nombres <- c(
  "Cartera de crédito",
  "Primas emitidas",
  "Transacciones rechazadas"
)

unidades <- c(
  "millones de pesos",
  "miles de millones de pesos",
  "transacciones por día"
)

frecuencias <- c("mensual", "trimestral", "diaria")
Hs <- c(6, 4, 7)
referentes <- c("Ingenuo", "Ingenuo estacional", "Ingenuo")

fmt <- function(x, d = 2) {
  if (length(x) == 0 || is.na(x[1]) || !is.finite(x[1])) {
    return("no disponible")
  }
  
  format(
    round(x[1], d),
    nsmall = d,
    big.mark = ".",
    decimal.mark = ","
  )
}

# Comprobar que existen resultados para los horizontes requeridos.
for (i in 1:3) {
  for (h in unique(c(1, Hs[i]))) {
    z <- ev[ev$serie == i & ev$horizonte == h, ]
    
    if (nrow(z) == 0 || all(is.na(z$RMSE))) {
      stop(
        "No hay RMSE válidos para la serie ", i,
        " y el horizonte ", h, "."
      )
    }
  }
}

# ------------------------------------------------------------
# 2. Funciones para obtener métodos y dibujar gráficos
# ------------------------------------------------------------

obtener_resultado <- function(i, h) {
  z <- ev[ev$serie == i & ev$horizonte == h, ]
  z <- z[is.finite(z$RMSE), ]
  
  if (nrow(z) == 0) {
    stop("No hay resultados válidos para serie ", i,
         ", horizonte ", h, ".")
  }
  
  ganador <- z[which.min(z$RMSE), , drop = FALSE]
  
  r <- z[z$metodo == referentes[i], , drop = FALSE]
  
  # Si el referente no aparece, no inventar un resultado.
  if (nrow(r) == 0) {
    r <- NULL
  } else {
    r <- r[1, , drop = FALSE]
  }
  
  list(ganador = ganador, referente = r)
}

# Los gráficos se construyen con los datos originales.
# El script busca los CSV en la carpeta datos/.
archivos <- c(
  "datos/STVQNQ-serie-1.csv",
  "datos/STVQNQ-serie-2.csv",
  "datos/STVQNQ-serie-3.csv"
)

# Alternativa si los CSV están en la raíz del repositorio.
if (!all(file.exists(archivos))) {
  archivos_alt <- c(
    "STVQNQ-serie-1.csv",
    "STVQNQ-serie-2.csv",
    "STVQNQ-serie-3.csv"
  )
  
  if (all(file.exists(archivos_alt))) {
    archivos <- archivos_alt
  } else {
    stop(
      "No se encuentran los tres CSV originales. ",
      "Revise la ubicación de los archivos de las series."
    )
  }
}

series <- lapply(archivos, function(f) {
  d <- read.csv(f, stringsAsFactors = FALSE)
  
  if (!all(c("fecha", "valor") %in% names(d))) {
    stop(
      "El archivo ", f,
      " debe contener las columnas fecha y valor."
    )
  }
  
  d$fecha <- as.Date(d$fecha)
  d$valor <- as.numeric(d$valor)
  d <- d[order(d$fecha), ]
  
  if (anyNA(d$fecha) || anyNA(d$valor)) {
    stop("Hay fechas o valores inválidos en ", f)
  }
  
  d
})

# ------------------------------------------------------------
# 3. Crear la nota de una página
# ------------------------------------------------------------

if (!dir.exists("informe")) {
  dir.create("informe", recursive = TRUE)
}

pdf(
  "informe/nota-interesados.pdf",
  width = 11.7,
  height = 8.3,
  onefile = TRUE
)

par(
  mfrow = c(1, 1),
  mar = c(0, 0, 0, 0),
  family = "sans"
)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# Encabezado
text(
  0.5, 0.972,
  "Pronósticos para apoyar decisiones",
  cex = 1.45, font = 2, col = "#17365D"
)

text(
  0.5, 0.940,
  "Equipo STVQNQ | Series de Tiempo | UNAL Medellín",
  cex = 0.78, col = "#555555"
)

text(
  0.02, 0.900,
  "Tres series, tres decisiones",
  adj = 0, cex = 1.0, font = 2, col = "#17365D"
)

# Posiciones de las tres figuras.
# Cada una muestra el historial observado de una serie.
pos_x <- c(0.02, 0.35, 0.68)
ancho <- 0.30

for (i in 1:3) {
  d <- series[[i]]
  
  # Marco y título de cada figura.
  rect(
    pos_x[i], 0.605,
    pos_x[i] + ancho, 0.855,
    border = "#D9E2F3"
  )
  
  text(
    pos_x[i] + ancho / 2, 0.832,
    nombres[i],
    cex = 0.77, font = 2, col = "#17365D"
  )
  
  # Gráfico de la serie histórica.
  # Se usa un gráfico compacto para que las tres figuras
  # quepan en la misma página.
  par(fig = c(
    pos_x[i] + 0.025,
    pos_x[i] + ancho - 0.015,
    0.640,
    0.805
  ), new = TRUE, mar = c(2.5, 3.1, 0.3, 0.3))
  
  plot(
    d$fecha, d$valor,
    type = "l",
    lwd = 1.1,
    col = "#2878B5",
    xlab = "",
    ylab = "",
    main = "",
    cex.axis = 0.60
  )
  
  if (i == 1) {
    mtext(
      "Millones de pesos",
      side = 2, line = 2.0, cex = 0.53
    )
  } else if (i == 2) {
    mtext(
      "Miles de millones",
      side = 2, line = 2.0, cex = 0.53
    )
  } else {
    mtext(
      "Rechazos por día",
      side = 2, line = 2.0, cex = 0.53
    )
  }
  
  mtext("Fecha", side = 1, line = 1.6, cex = 0.55)
}

# Restablecer el dispositivo gráfico completo.
par(fig = c(0, 1, 0, 1), new = FALSE, mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# ------------------------------------------------------------
# 4. Resultados comparativos y consecuencias para la decisión
# ------------------------------------------------------------

text(
  0.02, 0.570,
  "Resultados y lectura para la decisión",
  adj = 0, cex = 1.0, font = 2, col = "#17365D"
)

for (i in 1:3) {
  res <- obtener_resultado(i, Hs[i])
  b <- res$ganador
  r <- res$referente
  
  y <- 0.525 - (i - 1) * 0.145
  
  titulo <- paste0(
    nombres[i], " | horizonte: ", Hs[i],
    if (i == 1) " meses" else if (i == 2) " trimestres" else " días"
  )
  
  text(
    0.02, y, titulo,
    adj = 0, cex = 0.82, font = 2, col = "#17365D"
  )
  
  texto_ref <- if (is.null(r)) {
    paste0(
      "Referente: ", referentes[i],
      " (sin resultado disponible en este horizonte)."
    )
  } else {
    paste0(
      "Referente: ", referentes[i],
      "; RMSE = ", fmt(r$RMSE),
      " ", unidades[i], "."
    )
  }
  
  text(
    0.02, y - 0.030,
    paste0(
      "Ganador: ", b$metodo,
      "; RMSE = ", fmt(b$RMSE),
      " ", unidades[i], "."
    ),
    adj = 0, cex = 0.69
  )
  
  text(
    0.02, y - 0.056,
    texto_ref,
    adj = 0, cex = 0.66
  )
  
  if (!is.null(r)) {
    if (b$RMSE < r$RMSE) {
      mejora <- 100 * (r$RMSE - b$RMSE) / r$RMSE
      
      interpretacion <- paste0(
        "El RMSE fue ", fmt(mejora, 1),
        "% menor que el referente en la evaluación."
      )
    } else if (b$RMSE == r$RMSE) {
      interpretacion <- "El RMSE igualó al referente en la evaluación."
    } else {
      interpretacion <- paste0(
        "El método seleccionado no supera al referente por RMSE; ",
        "revise el criterio de selección."
      )
    }
  } else {
    interpretacion <- "No es posible comparar sin el RMSE del referente."
  }
  
  text(
    0.02, y - 0.082,
    interpretacion,
    adj = 0, cex = 0.64
  )
  
  if (i == 1) {
    decision <- paste0(
      "Decisión: usar la proyección como apoyo al fondeo de los ",
      Hs[i], " meses siguientes, con margen prudencial."
    )
  } else if (i == 2) {
    decision <- paste0(
      "Decisión: usarla como referencia para metas y reservas ",
      "trimestrales, con revisión al recibir nuevos datos."
    )
  } else {
    decision <- paste0(
      "Decisión: convertir el pronóstico en carga de trabajo ",
      "estimada y contrastarla con la capacidad de atención."
    )
  }
  
  text(
    0.02, y - 0.108,
    decision,
    adj = 0, cex = 0.64
  )
}

# ------------------------------------------------------------
# 5. Límites de interpretación
# ------------------------------------------------------------

text(
  0.02, 0.075,
  "Alcance y límites",
  adj = 0, cex = 0.90, font = 2, col = "#17365D"
)

text(
  0.02, 0.048,
  paste(
    "El RMSE resume errores de evaluación histórica en las unidades de cada serie;",
    "no es una garantía del error futuro."
  ),
  adj = 0, cex = 0.63
)

text(
  0.02, 0.025,
  paste(
    "Los resultados dependen del periodo y de los datos evaluados.",
    "Costos, capacidad operativa y nueva información deben complementar la decisión."
  ),
  adj = 0, cex = 0.63
)

text(
  0.02, 0.008,
  "Fuente: CSV de análisis y series originales del equipo STVQNQ.",
  adj = 0, cex = 0.52, col = "#555555"
)

dev.off()

message("Nota ejecutiva generada: informe/nota-interesados.pdf")
