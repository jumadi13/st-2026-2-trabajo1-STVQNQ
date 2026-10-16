acf_muestral <- function(y, m=min(floor(length(y)/4),24)) {
  if (length(y)<2 || m<0 || m>=length(y)) stop("m debe estar entre 0 y n-1")
  z <- y-mean(y); den <- sum(z^2); r <- numeric(m+1); r[1] <- 1
  for (k in seq_len(m)) r[k+1] <- sum(z[1:(length(y)-k)]*z[(k+1):length(y)])/den
  r
}

pacf_dl <- function(r) {
  # r contiene r_0, r_1, ..., r_m
  m <- length(r) - 1
  
  if (m < 0) {
    stop("El vector de autocorrelaciones no es válido.")
  }
  
  pacf <- numeric(m)
  
  if (m == 0) {
    return(pacf)
  }
  
  phi <- matrix(0, nrow = m, ncol = m)
  
  phi[1, 1] <- r[2]
  pacf[1] <- phi[1, 1]
  
  # Evitar que el bucle intente recorrer 2:1 cuando m = 1
  if (m >= 2) {
    for (k in 2:m) {
      num <- r[k + 1] -
        sum(phi[k - 1, 1:(k - 1)] * rev(r[2:k]))
      
      den <- 1 -
        sum(phi[k - 1, 1:(k - 1)] * r[2:k])
      
      if (abs(den) < .Machine$double.eps) {
        phi[k, k] <- NA_real_
      } else {
        phi[k, k] <- num / den
      }
      
      for (j in 1:(k - 1)) {
        phi[k, j] <- phi[k - 1, j] -
          phi[k, k] * phi[k - 1, k - j]
      }
      
      pacf[k] <- phi[k, k]
    }
  }
  
  pacf
}

ljung_box <- function(y, m = min(floor(length(y)/4),24)) {
  n <- length(y); r <- acf_muestral(y,m)[-1]; Q <- n*(n+2)*sum(r^2/(n-seq_len(m)))
  list(Q=Q, gl=m, p=1-pchisq(Q,m), m=m)
}
varianza_largo_plazo <- function(y, L = floor(4*(length(y)/100)^(2/9))) {
  n <- length(y); z <- y-mean(y); g0 <- mean(z^2); v <- g0
  for (k in seq_len(min(L,n-1))) { g <- sum(z[1:(n-k)]*z[(k+1):n])/n; v <- v+2*(1-k/(L+1))*g }
  max(v,0)
}
