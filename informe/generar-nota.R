
# Regenera la nota ejecutiva usando las salidas actuales de analisis/.
# Ejecutar desde la raíz: source("informe/generar-nota.R")
if (!file.exists("analisis/evaluacion-origen-movil.csv") ||
    !file.exists("analisis/resumen-descriptivo.csv")) {
  stop("No se encuentran los CSV de resultados. Ejecute primero analisis/correr-todo.R.")
}
ev <- read.csv("analisis/evaluacion-origen-movil.csv", stringsAsFactors=FALSE)
desc <- read.csv("analisis/resumen-descriptivo.csv", stringsAsFactors=FALSE)
nombres <- c("Cartera de crédito", "Primas emitidas", "Transacciones rechazadas")
Hs <- c(6, 4, 7)
referentes <- c("Ingenuo", "Ingenuo estacional", "Ingenuo")
fmt <- function(x, d=3) format(round(x, d), nsmall=d, big.mark=".", decimal.mark=",")
pdf("informe/nota-interesados.pdf", width=11.7, height=8.3, onefile=TRUE)
par(mar=c(0,0,0,0), family="sans")
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
text(.5,.96,"Pronósticos para apoyar decisiones", cex=1.45, font=2, col="#17365D")
text(.5,.925,"Equipo STVQNQ · Series de Tiempo · UNAL Medellín", cex=.8, col="#555555")
text(.02,.875,"Resultados de evaluación por origen móvil", adj=0, cex=1.0, font=2, col="#17365D")
headers <- c("Serie","Horizonte","Método seleccionado","RMSE ganador","Referente","RMSE referente")
xs <- c(.02,.25,.36,.61,.71,.86)
for (j in seq_along(headers)) text(xs[j],.835,headers[j],adj=0,cex=.66,font=2,col="#17365D")
yy <- .795
for (i in 1:3) {
  for (h in unique(c(1,Hs[i]))) {
    z <- ev[ev$serie==i & ev$horizonte==h,]
    b <- z[which.min(z$RMSE),]
    r <- z[z$metodo==referentes[i],]
    if (!nrow(r)) r <- z[1,]
    vals <- c(nombres[i], if (h==1) "1 paso" else paste(h,"pasos"),
              b$metodo, fmt(b$RMSE), referentes[i], fmt(r$RMSE))
    for (j in seq_along(vals)) text(xs[j],yy,vals[j],adj=0,cex=.66)
    yy <- yy-.035
  }
}
text(.02,yy-.01,"Lectura para la decisión",adj=0,cex=1.0,font=2,col="#17365D")
yy <- yy-.05
for (i in 1:3) {
  z <- ev[ev$serie==i & ev$horizonte==Hs[i],]
  b <- z[which.min(z$RMSE),]
  line <- if (i==1) paste0("Cartera: ",b$metodo," para seis meses; RMSE ",fmt(b$RMSE)," millones de pesos.")
  else if (i==2) paste0("Primas: ",b$metodo," para cuatro trimestres; RMSE ",fmt(b$RMSE)," miles de millones de pesos.")
  else paste0("Rechazos: media histórica ",fmt(desc$media[desc$serie==3],1)," por día; el promedio no determina por sí solo los turnos óptimos.")
  text(.02,yy,line,adj=0,cex=.72)
  yy <- yy-.035
}
text(.02,yy-.005,"Alcance y limitaciones",adj=0,cex=1.0,font=2,col="#17365D")
text(.02,yy-.045,"RMSE se expresa en las unidades de cada serie. Los ganadores dependen del período, la partición y la rejilla evaluada.",adj=0,cex=.69)
text(.02,yy-.075,"Las bandas del informe son heurísticas; la decisión final debe incorporar costos, capacidad y conocimiento del negocio.",adj=0,cex=.69)
text(.02,.035,"Fuente: analisis/evaluacion-origen-movil.csv y analisis/resumen-descriptivo.csv. Generada desde las salidas actuales.",adj=0,cex=.58,col="#555555")
dev.off()