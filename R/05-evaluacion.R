medidas <- function(real, pred) {
  ok <- is.finite(real) & is.finite(pred); if (!any(ok)) return(c(RMSE=NA_real_,MAD=NA_real_,MAPE=NA_real_))
  e <- real[ok]-pred[ok]
  c(RMSE=sqrt(mean(e^2)), MAD=mean(abs(e)), MAPE=mean(abs(e/real[ok]))*100)
}
optimizar <- function(y, metodo, rejilla) {
  n0 <- floor(.8*length(y)); yy <- y[1:n0]
  if (is.data.frame(rejilla) && ncol(rejilla)==0) rejilla <- list(list())
  if (is.data.frame(rejilla)) rejilla <- lapply(seq_len(nrow(rejilla)), function(i) as.list(rejilla[i,,drop=FALSE]))
  curva <- rep(NA_real_,length(rejilla)); mejor <- Inf; parmejor <- NULL
  for (i in seq_along(rejilla)) {
    fit <- try(do.call(metodo,c(list(y=yy),rejilla[[i]])),silent=TRUE)
    if (!inherits(fit,"try-error")) {
      ok <- is.finite(fit$yhat) & seq_along(yy) <= length(fit$yhat)
      if (sum(ok)>=5) { curva[i] <- mean((yy[ok]-fit$yhat[ok])^2); if (is.finite(curva[i]) && curva[i]<mejor) { mejor<-curva[i]; parmejor<-rejilla[[i]] } }
    }
  }
  if (is.null(parmejor)) stop(sprintf("No se encontró ajuste válido en la rejilla (n=%d; candidatos=%d). Revise los argumentos del método y las predicciones finitas.", length(y), length(rejilla)))
  list(parametros=parmejor,mse=mejor,curva=curva,rejilla=rejilla)
}
origen_movil <- function(y, metodo, parametros=list(), H=1, n0=floor(.8*length(y))) {
  if (H<1 || H!=as.integer(H) || n0>length(y)-H) stop("Horizonte u origen inválido")
  origenes <- n0:(length(y)-H); salida <- vector("list",length(origenes))
  for (i in seq_along(origenes)) {
    o <- origenes[i]; fit <- do.call(metodo,c(list(y=y[1:o]),parametros)); pr <- fit$pronosticar(H)
    salida[[i]] <- data.frame(origen=o,fecha_objetivo=o+H,real=y[o+H],pronostico=pr[H],error=y[o+H]-pr[H])
  }
  do.call(rbind,salida)
}
varianza_largo_plazo <- function(y, L=floor(4*(length(y)/100)^(2/9))) {
  n <- length(y); z <- y-mean(y); g0 <- sum(z^2)/n; v <- g0
  if (L>0) for (k in seq_len(min(L,n-1))) { g <- sum(z[1:(n-k)]*z[(k+1):n])/n; v <- v+2*(1-k/(L+1))*g }
  max(v,0)
}
mco_manual <- function(X,y,L=floor(4*(length(y)/100)^(2/9))) {
  X <- as.matrix(X); y <- as.numeric(y); n <- length(y)
  b <- solve(crossprod(X),crossprod(X,y)); e <- as.vector(y-X%*%b); bread <- solve(crossprod(X))
  sigma2 <- sum(e^2)/(n-ncol(X)); vcov_ordinaria <- sigma2*bread
  S <- crossprod(X*e)
  if (L>0) for (k in seq_len(min(L,n-1))) {
    w <- 1-k/(L+1); G <- crossprod(X[(k+1):n,,drop=FALSE]*e[(k+1):n],X[1:(n-k),,drop=FALSE]*e[1:(n-k)])
    S <- S+w*(G+t(G))
  }
  vcov_hac <- bread%*%S%*%bread
  list(coef=as.vector(b),se_ordinaria=sqrt(pmax(diag(vcov_ordinaria),0)),se_robusta=sqrt(pmax(diag(vcov_hac),0)),
       vcov_ordinaria=vcov_ordinaria,vcov_hac=vcov_hac,residuos=e,L=L)
}
jarque_bera_manual <- function(y) {
  n <- length(y); z <- (y-mean(y))/sqrt(mean((y-mean(y))^2)); S <- mean(z^3); K <- mean(z^4)
  JB <- n*(S^2/6+(K-3)^2/24); c(JB=JB,p=1-pchisq(JB,2),asimetria=S,curtosis=K)
}
durbin_watson_manual <- function(e) sum((e[-1]-e[-length(e)])^2)/sum(e^2)
prueba_media_manual <- function(e) {
  n <- length(e); se <- sd(e)/sqrt(n); t0 <- mean(e)/se
  c(media=mean(e),se=se,t=t0,gl=n-1,p=2*pt(-abs(t0),n-1),critico=qt(.975,n-1))
}
validar_errores <- function(e,m=min(floor(length(e)/4),24)) {
  list(media=mean(e),sd=sd(e),t=prueba_media_manual(e),lb=ljung_box(e,m),jb=jarque_bera_manual(e),dw=durbin_watson_manual(e))
}
