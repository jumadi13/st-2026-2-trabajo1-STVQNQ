# Genera una nota ejecutiva PDF de exactamente una página.
# Ejecutar desde la raíz: source("informe/generar-nota.R")

if (!file.exists("analisis/evaluacion-origen-movil.csv") ||
    !file.exists("analisis/resumen-descriptivo.csv")) {
  stop("Faltan resultados. Ejecute primero source('analisis/correr-todo.R').")
}

suppressPackageStartupMessages(library(grid))
ev <- read.csv("analisis/evaluacion-origen-movil.csv", stringsAsFactors=FALSE)
desc <- read.csv("analisis/resumen-descriptivo.csv", stringsAsFactors=FALSE)
pron <- read.csv("analisis/pronosticos-finales.csv", stringsAsFactors=FALSE)
if (!all(c("serie","horizonte","metodo","RMSE") %in% names(ev))) stop("Columnas de evaluación incompletas.")
if (!all(c("serie","media") %in% names(desc))) stop("Columnas descriptivas incompletas.")

nombres <- c("Cartera de crédito", "Primas emitidas", "Transacciones rechazadas")
unidades <- c("millones de pesos", "miles de millones", "rechazos/día")
frecuencias <- c("mensual", "trimestral", "diaria")
Hs <- c(6,4,7)
referentes <- c("Ingenuo", "Ingenuo estacional", "Ingenuo")
fmt <- function(x, d=2) {
  if (!length(x) || is.na(x[1]) || !is.finite(x[1])) return("N/D")
  format(round(x[1],d), nsmall=d, big.mark=".", decimal.mark=",")
}
obtener <- function(i,h,metodo=NULL) {
  z <- ev[ev$serie==i & ev$horizonte==h,,drop=FALSE]
  if (!is.null(metodo)) z <- z[z$metodo==metodo,,drop=FALSE]
  z <- z[is.finite(z$RMSE),,drop=FALSE]
  if (!nrow(z)) return(NULL)
  if (is.null(metodo)) z[which.min(z$RMSE),,drop=FALSE] else z[1,,drop=FALSE]
}

pdf("informe/nota-interesados.pdf", width=11.7, height=8.3, onefile=TRUE, useDingbats=FALSE)
grid.newpage()
# Una única página con dibujo vectorial: sin paneles base que creen páginas adicionales.
grid.rect(gp=gpar(fill="white",col=NA))
grid.text("PRONÓSTICOS PARA APOYAR DECISIONES", x=.5, y=.965,
          gp=gpar(fontsize=17,fontface="bold",col="#17365D"))
grid.text("Equipo STVQNQ · Series de Tiempo · UNAL Medellín · 2026-II", x=.5,y=.925,
          gp=gpar(fontsize=9,col="#555555"))
grid.text("Tres series, tres decisiones", x=.035,y=.885,just="left",
          gp=gpar(fontsize=12,fontface="bold",col="#17365D"))

# Tres minigráficos, cada uno dibujado en el mismo dispositivo/página.
for (i in 1:3) {
  d <- read.csv(sprintf("datos/STVQNQ-serie-%d.csv",i),stringsAsFactors=FALSE)
  d$fecha <- as.Date(d$fecha); d$valor <- as.numeric(d$valor)
  x0 <- c(.035,.355,.675)[i]; x1 <- x0+.29; y0 <- .665; y1 <- .845
  grid.roundrect(x=(x0+x1)/2,y=(y0+y1)/2,width=x1-x0,height=y1-y0,
                 r=unit(.02,"snpc"),gp=gpar(fill="#F8FAFD",col="#D9E2F3"))
  grid.text(nombres[i],x=(x0+x1)/2,y=.823,gp=gpar(fontsize=9,fontface="bold",col="#17365D"))
  vals <- tail(d$valor,min(nrow(d),65)); xx <- seq(x0+.025,x1-.018,length.out=length(vals))
  rng <- range(vals); if (diff(rng)==0) rng <- rng+c(-1,1)
  yy <- .692 + (vals-rng[1])/diff(rng)*.105
  grid.lines(x=xx,y=yy,gp=gpar(col="#2878B5",lwd=1.4))
  grid.text(paste0("Media: ",fmt(desc$media[desc$serie==i],1)," ",unidades[i]),
            x=(x0+x1)/2,y=.677,gp=gpar(fontsize=7,col="#444444"))
}

grid.text("Resultados en el horizonte de decisión",x=.035,y=.625,just="left",
          gp=gpar(fontsize=12,fontface="bold",col="#17365D"))
for (i in 1:3) {
  y <- c(.575,.465,.355)[i]
  b <- obtener(i,Hs[i]); r <- obtener(i,Hs[i],referentes[i])
  if (is.null(b)) stop("No hay resultado de evaluación para serie ",i," en horizonte ",Hs[i])
  title <- paste0(nombres[i]," · ",Hs[i]," ",c("meses","trimestres","días")[i])
  grid.text(title,x=.04,y=y,just="left",gp=gpar(fontsize=10,fontface="bold",col="#17365D"))
  grid.text(paste0("Método: ",b$metodo," | RMSE: ",fmt(b$RMSE)," ",unidades[i]),
            x=.055,y=y-.031,just="left",gp=gpar(fontsize=8.5))
  if (!is.null(r)) {
    mejora <- 100*(r$RMSE-b$RMSE)/r$RMSE
    comp <- if (is.finite(mejora) && mejora>0) paste0("RMSE ",fmt(mejora,1),"% menor que ",referentes[i],".") else if (isTRUE(all.equal(b$RMSE,r$RMSE))) paste0("RMSE igual al referente ",referentes[i],".") else paste0("No supera por RMSE al referente ",referentes[i],".")
  } else comp <- paste0("Sin comparación disponible con ",referentes[i],".")
  grid.text(comp,x=.055,y=y-.056,just="left",gp=gpar(fontsize=8,col="#444444"))
  dec <- c("Usar como apoyo para planear fondeo; conservar margen prudencial.",
           "Usar como referencia para metas y reservas; revisar al recibir nuevos datos.",
           "Usar para estimar carga diaria y contrastarla con la capacidad de revisión.")[i]
  grid.text(paste0("Aplicación: ",dec),x=.055,y=y-.079,just="left",gp=gpar(fontsize=7.8,col="#333333"))
  if (i<3) grid.lines(x=c(.04,.96),y=rep(y-.095,2),gp=gpar(col="#D9E2F3",lwd=.7))
}

grid.roundrect(x=.5,y=.145,width=.93,height=.075,r=unit(.012,"snpc"),
               gp=gpar(fill="#F3F6FA",col="#D9E2F3"))
grid.text("Alcance y límites",x=.055,y=.165,just="left",gp=gpar(fontsize=9,fontface="bold",col="#17365D"))
grid.text("El RMSE resume errores históricos y no garantiza el error futuro. Las bandas son heurísticas;",x=.055,y=.143,just="left",gp=gpar(fontsize=7.7))
grid.text("costos, capacidad operativa y nueva información deben complementar estas proyecciones.",x=.055,y=.124,just="left",gp=gpar(fontsize=7.7))
grid.text("Fuente: series originales y evaluación por orígenes móviles del equipo STVQNQ.",x=.035,y=.075,just="left",gp=gpar(fontsize=7,col="#555555"))
grid.text("Documento ejecutivo de una página · cifras expresadas en las unidades indicadas.",x=.965,y=.075,just="right",gp=gpar(fontsize=7,col="#555555"))
dev.off()
message("Nota ejecutiva generada en una sola página: informe/nota-interesados.pdf")
