# Métodos univariados. Cada yhat[t] se calcula usando solo y[1:(t-1)].
ajustar_media <- function(y) {
  yh <- rep(NA_real_, length(y)); for (t in 2:length(y)) yh[t] <- mean(y[1:(t-1)])
  list(yhat=yh, pronosticar=function(h) rep(mean(y), h), parametros=list())
}
ajustar_ingenuo <- function(y) {
  yh <- rep(NA_real_, length(y)); if (length(y)>1) yh[2:length(y)] <- y[1:(length(y)-1)]
  list(yhat=yh, pronosticar=function(h) rep(tail(y,1), h), parametros=list())
}
ajustar_ingenuo_estacional <- function(y, s) {
  if (length(y)<=s) stop("Se requiere al menos un ciclo completo")
  yh <- rep(NA_real_, length(y)); yh[(s+1):length(y)] <- y[1:(length(y)-s)]
  list(yhat=yh, pronosticar=function(h) vapply(seq_len(h), function(j) y[length(y)-s+(j-1)%%s+1], numeric(1)), parametros=list(s=s))
}
ajustar_mm <- function(y, k) {
  yh <- rep(NA_real_, length(y)); if (length(y)>k) for (t in (k+1):length(y)) yh[t] <- mean(y[(t-k):(t-1)])
  list(yhat=yh, pronosticar=function(h) rep(mean(tail(y,k)),h), parametros=list(k=k))
}
ajustar_ses <- function(y, alpha) {
  yh <- rep(NA_real_, length(y)); nivel <- y[1]
  for (t in 2:length(y)) { yh[t] <- nivel; nivel <- alpha*y[t]+(1-alpha)*nivel }
  list(yhat=yh, pronosticar=function(h) rep(nivel,h), parametros=list(alpha=alpha))
}
ajustar_dmm <- function(y, k) {
  yh <- rep(NA_real_, length(y)); n <- length(y)
  if (n >= 2*k) for (t in (2*k):n) {
    m1 <- mean(y[(t-k):(t-1)])
    medias <- vapply((t-2*k+1):(t-k), function(j) mean(y[j:(j+k-1)]), numeric(1))
    m2 <- mean(medias); a <- 2*m1-m2; b <- 2*(m1-m2)/(k-1); yh[t] <- a+b
  }
  pron <- function(h) {
    if (n < 2*k) return(rep(tail(y,1),h))
    m1 <- mean(tail(y,k)); medias <- vapply((n-2*k+1):(n-k), function(j) mean(y[j:(j+k-1)]), numeric(1))
    m2 <- mean(medias); a <- 2*m1-m2; b <- 2*(m1-m2)/(k-1); a+b*seq_len(h)
  }
  list(yhat=yh, pronosticar=pron, parametros=list(k=k))
}
ajustar_tendencia <- function(y, tipo=c("lineal","cuadratica","exponencial")) {
  tipo <- match.arg(tipo); n <- length(y); grado <- if (tipo=="cuadratica") 2 else 1
  Xfun <- function(t) if (grado==2) cbind(1,t,t^2) else cbind(1,t)
  respuesta <- function(z) if (tipo=="exponencial") log(z) else z
  yh <- rep(NA_real_,n); minimo <- grado+2
  if (n >= minimo) for (t in minimo:n) {
    tt <- seq_len(t-1); b <- solve(crossprod(Xfun(tt)), crossprod(Xfun(tt), respuesta(y[1:(t-1)])))
    pred <- sum(Xfun(t) * as.vector(b)); yh[t] <- if (tipo=="exponencial") exp(pred) else pred
  }
  tt <- seq_len(n); b <- solve(crossprod(Xfun(tt)), crossprod(Xfun(tt), respuesta(y)))
  pron <- function(h) { z <- Xfun(n+seq_len(h)) %*% b; if (tipo=="exponencial") as.vector(exp(z)) else as.vector(z) }
  list(yhat=yh, pronosticar=pron, parametros=list(tipo=tipo, coef=as.vector(b)))
}
ajustar_tendencia_estacional <- function(y, s) {
  n <- length(y); Xfun <- function(t) {
    temporada <- (t-1) %% s + 1
    cbind(1, t, 1.0*outer(temporada, 2:s, "=="))
  }
  yh <- rep(NA_real_,n); minimo <- s+2
  if (n >= minimo) for (t in minimo:n) {
    tt <- seq_len(t-1); X <- Xfun(tt); b <- solve(crossprod(X),crossprod(X,y[1:(t-1)]))
    yh[t] <- as.numeric(Xfun(t) %*% b)
  }
  X <- Xfun(seq_len(n)); b <- solve(crossprod(X),crossprod(X,y))
  list(yhat=yh, pronosticar=function(h) as.vector(Xfun(n+seq_len(h)) %*% b), parametros=list(s=s,coef=as.vector(b)))
}
ajustar_holt <- function(y, alpha, beta) {
  n <- length(y); yh <- rep(NA_real_,n); nivel <- y[1]; tendencia <- if (n>1) y[2]-y[1] else 0
  if (n>1) for (t in 2:n) {
    yh[t] <- nivel+tendencia; anterior <- nivel
    nivel <- alpha*y[t]+(1-alpha)*(nivel+tendencia)
    tendencia <- beta*(nivel-anterior)+(1-beta)*tendencia
  }
  list(yhat=yh, pronosticar=function(h) nivel+tendencia*seq_len(h), parametros=list(alpha=alpha,beta=beta))
}
ajustar_hw <- function(y, s, alpha, beta, gamma) {
  n <- length(y); if (n < 2*s) stop("HW requiere al menos dos ciclos")
  nivel <- mean(y[1:s]); tendencia <- (mean(y[(s+1):(2*s)])-nivel)/s
  est <- y[1:s]-nivel; yh <- rep(NA_real_,n)
  for (t in (s+1):n) {
    j <- (t-s-1) %% s + 1; yh[t] <- nivel+tendencia+est[j]; anterior <- nivel
    nivel <- alpha*(y[t]-est[j])+(1-alpha)*(nivel+tendencia)
    tendencia <- beta*(nivel-anterior)+(1-beta)*tendencia
    est[j] <- gamma*(y[t]-nivel)+(1-gamma)*est[j]
  }
  pron <- function(h) vapply(seq_len(h), function(j) nivel+j*tendencia+est[(n-s+j-1)%%s+1], numeric(1))
  list(yhat=yh, pronosticar=pron, parametros=list(s=s,alpha=alpha,beta=beta,gamma=gamma))
}
ajustar_hw_mult <- function(y, s, alpha, beta, gamma) {
  n <- length(y); if (any(y<=0)) stop("HW multiplicativo requiere valores positivos")
  if (n < 2*s) stop("HW requiere al menos dos ciclos")
  nivel <- mean(y[1:s]); tendencia <- (mean(y[(s+1):(2*s)])-nivel)/s
  est <- y[1:s]/nivel; yh <- rep(NA_real_,n)
  for (t in (s+1):n) {
    j <- (t-s-1) %% s + 1; yh[t] <- (nivel+tendencia)*est[j]; anterior <- nivel
    nivel <- alpha*(y[t]/est[j])+(1-alpha)*(nivel+tendencia)
    tendencia <- beta*(nivel-anterior)+(1-beta)*tendencia
    est[j] <- gamma*(y[t]/nivel)+(1-gamma)*est[j]
  }
  pron <- function(h) vapply(seq_len(h), function(j) (nivel+j*tendencia)*est[(n-s+j-1)%%s+1], numeric(1))
  list(yhat=yh, pronosticar=pron, parametros=list(s=s,alpha=alpha,beta=beta,gamma=gamma))
}
