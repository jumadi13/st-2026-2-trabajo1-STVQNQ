graficar_serie <- function(x, nombre, unidad, archivo) {
  png(archivo, width = 1100, height = 650, res = 130)
  plot(x$fecha, x$valor, type = "l", xlab = "Fecha", ylab = unidad, main = nombre)
  mtext(paste("N =", nrow(x)), side = 1, line = 4, adj = 1)
  dev.off()
}
grafico_estacional <- function(y, s, archivo) {
  n <- length(y); ciclos <- ceiling(n/s)
  mat <- matrix(NA_real_, nrow = s, ncol = ciclos)
  for (i in seq_len(n)) mat[(i-1) %% s + 1, (i-1) %/% s + 1] <- y[i]
  png(archivo, 1000, 600); matplot(seq_len(s), mat, type = "l", lty = 1, xlab = "Índice estacional", ylab = "Valor", main = paste("Gráfico estacional, s =", s)); dev.off()
}
grafico_rezagos <- function(y, rezagos = c(1, 2, 4, 7, 12), archivo) {
  png(archivo, 1100, 700); par(mfrow = c(2,3))
  for (k in rezagos) if (k < length(y)) plot(y[1:(length(y)-k)], y[(k+1):length(y)], xlab = "y[t-k]", ylab = "y[t]", main = paste("Rezago", k))
  dev.off(); par(mfrow = c(1,1))
}
correlograma <- function(y, m = min(floor(length(y)/4), 24), archivo) {
  r <- acf_muestral(y, m); png(archivo, 1000, 550)
  plot(0:m, r, type="h", xlab="Rezago", ylab="ACF", main="Correlograma", ylim=c(-1,1)); abline(h=c(-1.96,1.96)/sqrt(length(y)), lty=2); dev.off()
}
descomposicion_clasica <- function(y, s, tipo = c("aditiva", "multiplicativa")) {
  tipo <- match.arg(tipo); n <- length(y); tendencia <- rep(NA_real_, n)
  # Media móvil centrada para período par; para impar, media móvil simple.
  if (s %% 2 == 0) {
    for (i in (s/2+1):(n-s/2)) {
      idx <- (i-s/2):(i+s/2); w <- rep(1,s+1); w[c(1,s+1)] <- 0.5
      tendencia[i] <- sum(y[idx]*w)/s
    }
  } else {
    for (i in ((s+1)/2):(n-(s-1)/2)) tendencia[i] <- mean(y[(i-(s-1)/2):(i+(s-1)/2)])
  }
  residuo <- if (tipo == "aditiva") y-tendencia else y/tendencia
  indices <- sapply(seq_len(s), function(j) mean(residuo[seq(j,n,by=s)], na.rm=TRUE))
  if (tipo == "aditiva") indices <- indices-mean(indices) else indices <- indices/mean(indices)
  list(tendencia=tendencia, indices=indices, residuo=if(tipo=="aditiva") residuo-indices[(seq_len(n)-1)%%s+1] else residuo/indices[(seq_len(n)-1)%%s+1], tipo=tipo)
}
