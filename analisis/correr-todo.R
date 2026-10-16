# Flujo reproducible: ejecutar desde la raíz con source("analisis/correr-todo.R").
if (!dir.exists("R") || !dir.exists("datos") || !dir.exists("analisis"))
  stop("Ejecute source('analisis/correr-todo.R') desde la raíz del repositorio STVQNQ.")
for (f in list.files("R", pattern="\\.R$", full.names=TRUE)) source(f)
dir.create("figs", showWarnings=FALSE)
config <- list(
  list(freq="mensual",H=6,nombre="Cartera de crédito",unidad="millones de pesos",s=12),
  list(freq="trimestral",H=4,nombre="Primas emitidas",unidad="miles de millones de pesos",s=4),
  list(freq="diaria",H=7,nombre="Transacciones rechazadas",unidad="transacciones por día",s=7)
)
# Rejillas prescritas por el enunciado. Todas se eligen exclusivamente en el 80 % inicial.
rejilla <- function(nombre,s) {
  if (nombre=="Media móvil") return(lapply(2:12,function(k) list(k=k)))
  if (nombre=="SES") return(lapply(seq(.02,.98,by=.02),function(a) list(alpha=a)))
  if (nombre=="Doble media móvil") return(lapply(3:12,function(k) list(k=k)))
  if (nombre=="Holt") return(lapply(seq_len(400),function(i) list(alpha=((i-1)%/%20+1)*.05,beta=((i-1)%%20+1)*.05)))
  if (nombre %in% c("HW aditivo","HW multiplicativo")) return(lapply(seq_len(1000),function(i) list(s=s,alpha=(((i-1)%/%100)+1)*.1,beta=(((i-1)%/%10)%%10+1)*.1,gamma=((i-1)%%10+1)*.1)))
  list(list())
}
resultados <- vector("list",3); filas_eval <- list(); filas_opt <- list(); filas_pron <- list(); filas_curva <- list(); filas_err_h <- list(); filas_reg <- list(); filas_reg_diag <- list()
for (i in 1:3) {
  cfg <- config[[i]]; x <- leer_serie(sprintf("datos/STVQNQ-serie-%d.csv",i),cfg$freq)
  y <- x$valor; T <- length(y); n0 <- floor(.8*T); H <- cfg$H; s <- cfg$s; m <- min(floor(T/4),24)
  if (T != c(150,128,160)[i]) stop(sprintf("La serie %d tiene T=%d y no coincide con el archivo asignado.",i,T))
  if (n0 != c(120,102,128)[i] || m!=24) stop("Falló un control de partición")
  graficar_serie(x,paste("Serie",i,"—",cfg$nombre),cfg$unidad,sprintf("figs/s%d_serie.png",i))
  grafico_rezagos(y,c(1,2,4,7,12),sprintf("figs/s%d_rezagos.png",i))
  correlograma(y,m,sprintf("figs/s%d_acf.png",i))
  grafico_estacional(y,s,sprintf("figs/s%d_estacional.png",i))
  # Verificaciones de implementación: las funciones base solo se usan como contraste.
  stopifnot(max(abs(acf_muestral(y,m)-as.numeric(acf(y,lag.max=m,plot=FALSE)$acf)))<1e-10)
  stopifnot(max(abs(pacf_dl(acf_muestral(y,m))-as.numeric(pacf(y,lag.max=m,plot=FALSE)$acf)))<1e-10)
  stopifnot(abs(ljung_box(y,m)$Q-as.numeric(Box.test(y,lag=m,type="Ljung-Box")$statistic))<1e-8)
  stopifnot(max(abs(diferenciar(y,1,0,s)-diff(y)))<1e-10)
  stopifnot(max(abs(diferenciar(y,0,1,s)-(y[(s+1):T]-y[1:(T-s)])))<1e-10)
  X <- cbind(1,seq_along(y)); mc <- mco_manual(X,y)
  stopifnot(max(abs(mc$coef-as.numeric(coef(lm(y~seq_along(y))))))<1e-8)
  # Pruebas cortas de convenciones de pronóstico y calentamiento.
  z <- c(10,12,15,14,18,20)
  stopifnot(is.na(ajustar_media(z)$yhat[1]), ajustar_media(z)$yhat[3]==mean(z[1:2]))
  stopifnot(ajustar_ingenuo(z)$yhat[4]==z[3], ajustar_mm(z,2)$yhat[4]==mean(z[2:3]))
  stopifnot(ajustar_ingenuo_estacional(z,2)$yhat[3]==z[1])
  stopifnot(abs(ajustar_ses(z,.5)$yhat[3]-(.5*z[2]+.5*z[1]))<1e-12)
  stopifnot(abs(ajustar_dmm(z,3)$yhat[6]-19.2222222222)<1e-8)
  stopifnot(abs(ajustar_tendencia(c(10,12,14,16),"lineal")$pronosticar(1)-18)<1e-8)
  stopifnot(abs(ajustar_tendencia(c(1,4,9,16,25),"cuadratica")$pronosticar(1)-36)<1e-8)
  stopifnot(abs(ajustar_tendencia(c(2,4,8,16),"exponencial")$pronosticar(1)-32)<1e-8)
  zs <- c(11,22,13,24,15,26,17,28)
  stopifnot(abs(ajustar_tendencia_estacional(zs,2)$pronosticar(1)-19)<1e-8)
  stopifnot(abs(ajustar_holt(z,.5,.5)$yhat[3]-14)<1e-8)
  stopifnot(abs(ajustar_hw(c(10,12,15,14),2,.5,.5,.5)$yhat[3]-11.75)<1e-8)
  stopifnot(abs(ajustar_hw_mult(c(10,12,15,14),2,.5,.5,.5)$yhat[3]-(12.75*10/11))<1e-8)
  # Métodos del catálogo, incluyendo contrastes deliberados.
  specs <- list(
    "Media simple"=list(fn=ajustar_media), "Ingenuo"=list(fn=ajustar_ingenuo),
    "Ingenuo estacional"=list(fn=function(y) ajustar_ingenuo_estacional(y,s)),
    "Media móvil"=list(fn=ajustar_mm), "SES"=list(fn=ajustar_ses), "Doble media móvil"=list(fn=ajustar_dmm),
    "Tendencia lineal"=list(fn=function(y) ajustar_tendencia(y,"lineal")),
    "Tendencia cuadrática"=list(fn=function(y) ajustar_tendencia(y,"cuadratica")),
    "Tendencia exponencial"=list(fn=function(y) ajustar_tendencia(y,"exponencial")),
    "Tendencia estacional"=list(fn=function(y) ajustar_tendencia_estacional(y,s)),
    "Holt"=list(fn=ajustar_holt), "HW aditivo"=list(fn=ajustar_hw), "HW multiplicativo"=list(fn=ajustar_hw_mult))
  fits <- list(); metricas <- list()
  for (nm in names(specs)) {
    fn <- specs[[nm]]$fn; grid <- rejilla(nm,s)
    # Los métodos con constantes fijas usan una única configuración vacía.
    if (nm=="Ingenuo estacional") grid <- list(list())
    opt <- optimizar(y,fn,grid); par <- opt$parametros
    fits[[nm]] <- list(fn=fn,parametros=par,opt=opt)
    for (j in seq_along(grid)) {
      pp <- grid[[j]]; val <- opt$curva[j]
      etiqueta_pp <- if (length(pp)) paste(names(pp),unlist(pp),collapse=";") else "fijo"
      filas_curva[[length(filas_curva)+1]] <- data.frame(serie=i,metodo=nm,parametros=etiqueta_pp,MSE=val)
    }
    etiqueta_par <- if (length(par)) paste(names(par),unlist(par),collapse=";") else "fijo"
    filas_opt[[length(filas_opt)+1]] <- data.frame(serie=i,metodo=nm,parametros=etiqueta_par,MSE_entrenamiento=opt$mse)
    for (hh in unique(c(1,H))) {
      ev <- origen_movil(y,fn,par,H=hh,n0=n0); mm <- medidas(ev$real,ev$pronostico)
      filas_eval[[length(filas_eval)+1]] <- data.frame(serie=i,metodo=nm,horizonte=hh,n=nrow(ev),RMSE=mm["RMSE"],MAD=mm["MAD"],MAPE=mm["MAPE"],parametros=etiqueta_par)
      metricas[[paste(nm,hh,sep="_")]] <- ev
    }
  }
  evtab <- do.call(rbind,filas_eval); evserie <- evtab[evtab$serie==i,]
  # Ganador a H: se evalúa y reajusta después con la serie completa.
  ganador <- evserie[evserie$horizonte==H,]; ganador <- ganador[which.min(ganador$RMSE),]
  nm <- ganador$metodo; fn <- fits[[nm]]$fn; par <- fits[[nm]]$parametros
  fitfinal <- do.call(fn,c(list(y=y),par)); pred <- fitfinal$pronosticar(H)
  err_sd <- numeric(H)
  for (hh in seq_len(H)) {
    evh <- origen_movil(y,fn,par,H=hh,n0=n0); err_sd[hh] <- sd(evh$error)
    filas_err_h[[length(filas_err_h)+1]] <- data.frame(serie=i,metodo=nm,h=hh,n=nrow(evh),media_error=mean(evh$error),sd_error=sd(evh$error),RMSE=sqrt(mean(evh$error^2)))
  }
  if (cfg$freq=="mensual") fechas_fut <- seq(x$fecha[T],by="month",length.out=H+1)[-1]
  if (cfg$freq=="trimestral") fechas_fut <- seq(x$fecha[T],by="3 months",length.out=H+1)[-1]
  if (cfg$freq=="diaria") fechas_fut <- seq(x$fecha[T]+1,by="day",length.out=H)
  filas_pron[[length(filas_pron)+1]] <- data.frame(serie=i,metodo=nm,h=seq_len(H),fecha=fechas_fut,pronostico=pred,sd_error=err_sd,limite_inferior=pred-1.96*err_sd,limite_superior=pred+1.96*err_sd)
  # Figura del pronóstico para la decisión.
  png(sprintf("figs/s%d_pronostico.png",i),width=1100,height=650,res=130)
  plot(tail(x$fecha, min(30,T)),tail(y,min(30,T)),type="l",xlab="Fecha",ylab=cfg$unidad,main=paste("Serie",i,"— pronóstico a H =",H))
  lines(fechas_fut,pred,col=2,lwd=2); points(fechas_fut,pred,col=2,pch=19)
  lines(fechas_fut,pred-1.96*err_sd,col=4,lty=2); lines(fechas_fut,pred+1.96*err_sd,col=4,lty=2)
  legend("topleft",legend=c("Observado","Pronóstico","Banda heurística ±1,96 DE"),col=c(1,2,4),lty=c(1,1,2),bty="n"); dev.off()
  # Diagnóstico del ganador a un paso, sobre errores de origen móvil.
  winner1 <- evserie[evserie$horizonte==1,]; winner1 <- winner1[which.min(winner1$RMSE),]
  e1 <- metricas[[paste(winner1$metodo,1,sep="_")]]$error
  # Regresiones deterministas: errores estándar ordinarios/HAC y diagnósticos de residuos.
  tt <- seq_len(T); temporada <- (tt-1) %% s + 1
  diseños <- list(
    "Tendencia lineal"=list(X=cbind(Intercepto=1,t=tt),yy=y),
    "Tendencia cuadrática"=list(X=cbind(Intercepto=1,t=tt,t2=tt^2),yy=y),
    "Tendencia exponencial"=list(X=cbind(Intercepto=1,t=tt),yy=log(y)),
    "Tendencia estacional"=list(X=cbind(Intercepto=1,t=tt,1.0*outer(temporada,2:s,"==")),yy=y)
  )
  colnames(diseños[["Tendencia estacional"]]$X) <- c("Intercepto","t",paste0("estacion_",2:s))
  for (nmreg in names(diseños)) {
    Xreg <- diseños[[nmreg]]$X; yreg <- diseños[[nmreg]]$yy; reg <- mco_manual(Xreg,yreg); glreg <- T-ncol(Xreg)
    treg <- reg$coef/reg$se_robusta; preg <- 2*pt(-abs(treg),glreg)
    for (jreg in seq_along(reg$coef)) filas_reg[[length(filas_reg)+1]] <- data.frame(serie=i,modelo=nmreg,coeficiente=colnames(Xreg)[jreg],estimacion=reg$coef[jreg],SE_ordinario=reg$se_ordinaria[jreg],SE_HAC=reg$se_robusta[jreg],t_HAC=treg[jreg],gl=glreg,p=preg[jreg],t_critico=qt(.975,glreg),decision=if (preg[jreg]<.05) "rechazar H0" else "no rechazar H0",lectura=if (preg[jreg]<.05) if (reg$coef[jreg]>0) "efecto positivo significativo" else "efecto negativo significativo" else "sin evidencia de efecto distinto de cero")
    lbreg <- ljung_box(reg$residuos,m); jbreg <- jarque_bera_manual(reg$residuos)
    filas_reg_diag[[length(filas_reg_diag)+1]] <- data.frame(serie=i,modelo=nmreg,n=T,parametros=ncol(Xreg),L=reg$L,LB_Q=lbreg$Q,LB_gl=m,LB_p=lbreg$p,JB=jbreg["JB"],JB_p=jbreg["p"],decision_LB=if (lbreg$p<.05) "rechazar H0" else "no rechazar H0",decision_JB=if (jbreg["p"]<.05) "rechazar H0" else "no rechazar H0",DW=durbin_watson_manual(reg$residuos),r1=acf_muestral(reg$residuos,1)[2],lectura_DW=if (acf_muestral(reg$residuos,1)[2]>0) "dependencia positiva de primer orden" else "dependencia negativa de primer orden")
  }
  # Dependencia de la transformación elegida y figura de descomposición cuando hay estacionalidad sustentada.
  zdep <- if (i==1) diferenciar(y,1,0,s) else if (i==2) log(y[(s+1):T])-log(y[1:(T-s)]) else y
  rz <- acf_muestral(zdep,min(24,length(zdep)-1)); pz <- pacf_dl(rz)
  png(sprintf("figs/s%d_dependencia-transformada.png",i),width=1100,height=650,res=130); par(mfrow=c(2,1))
  plot(seq_along(rz)-1,rz,type="h",xlab="Rezago",ylab="ACF",main="ACF de la transformación elegida"); abline(h=c(-1.96,1.96)/sqrt(length(zdep)),lty=2)
  bw <- vapply(seq_len(m),function(k) 1.96*sqrt((1+2*(if (k>1) sum(rz[2:k]^2) else 0))/length(zdep)),numeric(1))
  lines(seq_len(m),bw,lty=3); lines(seq_len(m),-bw,lty=3)
  plot(seq_along(pz),pz,type="h",xlab="Rezago",ylab="PACF",main="PACF de la transformación elegida"); abline(h=c(-1.96,1.96)/sqrt(length(zdep)),lty=2); dev.off(); par(mfrow=c(1,1))
  if (i==2) {
    dc <- descomposicion_clasica(y,s,"multiplicativa"); png("figs/s2_descomposicion.png",width=1100,height=800,res=130); par(mfrow=c(4,1))
    plot(x$fecha,y,type="l",main="Descomposición multiplicativa: primas",xlab="Fecha",ylab=cfg$unidad)
    plot(x$fecha,dc$tendencia,type="l",main="Tendencia estimada",xlab="Fecha",ylab=cfg$unidad)
    plot(seq_len(s),dc$indices,type="b",main="Índices estacionales normalizados",xlab="Trimestre",ylab="Índice")
    plot(x$fecha,dc$residuo,type="l",main="Residuo",xlab="Fecha",ylab="Razón residual"); dev.off(); par(mfrow=c(1,1))
  }
  resultados[[i]] <- list(datos=x,nombre=cfg$nombre,unidad=cfg$unidad,frecuencia=cfg$freq,T=T,n0=n0,H=H,s=s,m=m,
    fecha_fin_entrenamiento=x$fecha[n0],fecha_inicio_prueba=x$fecha[n0+1],media=mean(y),desviacion=sd(y),varianza=var(y),minimo=min(y),maximo=max(y),
    transformaciones=list(original=y,diferencia_regular=diferenciar(y,1,0,s),diferencia_estacional=diferenciar(y,0,1,s),combinada=diferenciar(y,1,1,s),
      log_diferencia_regular=log(y[2:T])-log(y[1:(T-1)]),log_diferencia_estacional=log(y[(s+1):T])-log(y[1:(T-s)])),
    acf=acf_muestral(zdep,m),pacf=pacf_dl(acf_muestral(zdep,m)),ljung=ljung_box(zdep,m),fits=fits,evaluaciones=metricas,ganador_h1=winner1$metodo,ganador_H=nm,
    errores_ganador_h1=e1,validacion=validar_errores(e1,min(floor(length(e1)/4),24)),pronostico=pred,fechas_pronostico=fechas_fut,sd_error_h=err_sd)
}
write.csv(do.call(rbind,filas_eval),"analisis/evaluacion-origen-movil.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_opt),"analisis/optimizacion-mse.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_curva),"analisis/curvas-MSE.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_pron),"analisis/pronosticos-finales.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_err_h),"analisis/errores-por-horizonte.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_reg),"analisis/regresiones-coeficientes.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_reg_diag),"analisis/regresiones-diagnosticos.csv",row.names=FALSE)
filas_validacion <- lapply(seq_along(resultados), function(i) {
  z <- resultados[[i]]; v <- z$validacion
  data.frame(serie=i,ganador_h1=z$ganador_h1,n_errores=length(z$errores_ganador_h1),media_error=v$media,sd_error=v$sd,
    t=v$t["t"],p_t=v$t["p"],LB_Q=v$lb$Q,LB_gl=v$lb$gl,LB_p=v$lb$p,JB=v$jb["JB"],JB_p=v$jb["p"],DW=v$dw)
})
write.csv(do.call(rbind,filas_validacion),"analisis/validacion-errores.csv",row.names=FALSE)
for (i in seq_along(resultados)) {
  z <- resultados[[i]]; e <- z$errores_ganador_h1; m <- min(12,length(e)-1); re <- acf_muestral(e,m)
  png(sprintf("figs/s%d_errores.png",i),width=1000,height=650,res=130); par(mfrow=c(2,1))
  plot(seq_along(e),e,type="b",xlab="Origen móvil",ylab="Error",main=paste("Errores a un paso —",z$ganador_h1)); abline(h=0,lty=2)
  plot(0:m,re,type="h",xlab="Rezago",ylab="ACF de errores",main="Correlograma de errores"); abline(h=c(-1.96,1.96)/sqrt(length(e)),lty=2); dev.off(); par(mfrow=c(1,1))
}
# Regenerar también todas las tablas derivadas consumidas por el informe.
filas_desc <- list(); filas_trans <- list(); filas_raices <- list(); filas_dep <- list(); filas_bloques <- list(); filas_est <- list()
for (i in 1:3) {
  z <- resultados[[i]]
  cfg <- config[[i]]
  x <- z$datos; y <- x$valor; T <- length(y); s <- z$s; n0 <- z$n0
  filas_desc[[i]] <- data.frame(serie=i,nombre=cfg$nombre,unidad=cfg$unidad,frecuencia=cfg$freq,
    inicio=min(x$fecha),fin=max(x$fecha),T=T,media=mean(y),desv_est=sd(y),varianza=var(y),
    minimo=min(y),maximo=max(y),mediana=median(y),n0=n0,fecha_fin_entrenamiento=x$fecha[n0],
    fecha_inicio_prueba=x$fecha[n0+1],H=z$H,m=z$m,origenes_h1=T-n0,origenes_H=T-z$H-n0+1,
    acf1=acf_muestral(y, min(1,T-1))[2],acf4=acf_muestral(y,min(4,T-1))[5],
    acf7=acf_muestral(y,min(7,T-1))[8],acf12=acf_muestral(y,min(12,T-1))[13])
  cand <- list("Original"=y,"Diferencia regular"=diferenciar(y,1,0,s),
    "Diferencia estacional"=diferenciar(y,0,1,s),"Combinada"=diferenciar(y,1,1,s),
    "Log + diferencia regular"=diff(log(y)),
    "Log + diferencia estacional"=diff(log(y),lag=s),
    "Log + combinada"=diferenciar(log(y),1,1,s))
  polys <- list("Original"=1,"Diferencia regular"=c(1,-1),
    "Diferencia estacional"=c(1,rep(0,s-1),-1),"Combinada"=c(1,-1,rep(0,s-2),-1,1),
    "Log + diferencia regular"=c(1,-1),"Log + diferencia estacional"=c(1,rep(0,s-1),-1),
    "Log + combinada"=c(1,-1,rep(0,s-2),-1,1))
  for (nmtr in names(cand)) {
    v <- cand[[nmtr]]; filas_trans[[length(filas_trans)+1]] <- data.frame(serie=i,candidata=nmtr,
      polinomio=paste(polys[[nmtr]],collapse=" "),n=length(v),media=mean(v),varianza=var(v),r1=acf_muestral(v,1)[2])
    if (nmtr!="Original") {
      rr <- polyroot(polys[[nmtr]])
      for (r0 in rr) filas_raices[[length(filas_raices)+1]] <- data.frame(serie=i,candidata=nmtr,
        coeficientes=paste(polys[[nmtr]],collapse=";"),raiz=as.character(r0),real=Re(r0),imaginaria=Im(r0),modulo=Mod(r0))
    }
  }
  zd <- if (i==1) diferenciar(y,1,0,s) else if (i==2) diff(log(y),lag=s) else y
  nzd <- length(zd); mm <- min(floor(nzd/4),24); lj <- ljung_box(zd,mm); L <- floor(4*(nzd/100)^(2/9)); lrv <- varianza_largo_plazo(zd,L)
  se0 <- sd(zd)/sqrt(nzd); selp <- sqrt(lrv/nzd)
  filas_dep[[i]] <- data.frame(serie=i,n=nzd,media=mean(zd),varianza=var(zd),r1=acf_muestral(zd,1)[2],m=mm,L=L,
    LB_Q=lj$Q,LB_gl=lj$gl,LB_p=lj$p,LRV_Bartlett=lrv,SE_ingenuo=se0,
    IC_ingenuo_inf=mean(zd)-1.96*se0,IC_ingenuo_sup=mean(zd)+1.96*se0,
    SE_largo_plazo=selp,IC_LP_inf=mean(zd)-1.96*selp,IC_LP_sup=mean(zd)+1.96*selp,
    ACF_lag2=acf_muestral(zd,min(2,nzd-1))[3],PACF_lag1=pacf_dl(acf_muestral(zd,1))[1])
  nb <- 4; cuts <- split(seq_along(zd), cut(seq_along(zd),breaks=nb,labels=FALSE))
  for (b in seq_along(cuts)) { v <- zd[cuts[[b]]]; filas_bloques[[length(filas_bloques)+1]] <- data.frame(serie=i,transformacion=if(i==1)"Diferencia regular" else if(i==2)"Log + diferencia estacional" else "Original",bloque=b,n=length(v),media=mean(v),varianza=var(v)) }
  temporada <- (seq_len(T)-1) %% s + 1; amp <- tapply(y,temporada,mean); fitlin <- lm(y~seq_len(T)); an <- anova(lm(resid(fitlin)~factor(temporada)))
  filas_est[[i]] <- data.frame(serie=i,periodo_candidato=s,acf_rezago_s=acf_muestral(y,s)[s+1],
    media_estacional_min=min(amp),media_estacional_max=max(amp),amplitud_medias=max(amp)-min(amp),
    F_anova_sobre_residuo_lineal=an$`F value`[1],p_valor=an$`Pr(>F)`[1])
  # Curvas MSE: un gráfico por serie; métodos fijos se separan de los ajustables.
  cv <- do.call(rbind,filas_curva); cv <- cv[cv$serie==i & is.finite(cv$MSE),,drop=FALSE]
  png(sprintf("figs/s%d_curvas_MSE.png",i),width=1200,height=750,res=130)
  par(mar=c(9,5,3,1)); if(nrow(cv)) {
    cv$etiqueta <- paste(cv$metodo,cv$parametros,sep="\n")
    cv <- cv[order(cv$MSE),]; cv <- head(cv, min(35,nrow(cv)))
    barplot(cv$MSE,names.arg=cv$metodo,las=2,cex.names=.58,
      ylab="MSE de un paso (entrenamiento)",main=paste("Serie",i,"— mejores configuraciones por MSE"),col="steelblue")
  } else { plot.new(); title(main=paste("Serie",i,"— curvas MSE")); text(.5,.5,"No hay valores MSE finitos") }
  dev.off()
}
write.csv(do.call(rbind,filas_desc),"analisis/resumen-descriptivo.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_trans),"analisis/transformaciones-candidatas.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_raices),"analisis/raices-operadores.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_dep),"analisis/dependencia-transformada.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_bloques),"analisis/bloques-estacionariedad.csv",row.names=FALSE)
write.csv(do.call(rbind,filas_est),"analisis/evidencia-estacionalidad.csv",row.names=FALSE)

saveRDS(resultados,"analisis/resultados.rds")
writeLines(capture.output(sessionInfo()),"sesion-info.txt")
# Mantener los entregables sincronizados con los CSV/RDS recién generados.
source("informe/generar-nota.R")
if (nzchar(Sys.which("quarto"))) {
  salida_quarto <- system2("quarto", c("render", "informe/informe.qmd"),
                           stdout=TRUE, stderr=TRUE)
  estado_quarto <- attr(salida_quarto, "status")
  if (!is.null(estado_quarto) && estado_quarto != 0) {
    stop(paste(c("Falló el render de Quarto:", salida_quarto), collapse="\\n"))
  }
  message("HTML actualizado con los resultados recién generados.")
} else {
  warning("Quarto no está instalado o no está en PATH; el HTML no se actualizó. Ejecute: quarto render informe/informe.qmd")
}
message("Flujo R completado; se regeneraron RDS, CSV, figuras, sesion-info.txt y nota-interesados.pdf.")
