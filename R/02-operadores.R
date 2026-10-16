diferenciar <- function(y, d = 0, D = 0, s = 1) {
  if (d < 0 || D < 0 || d != as.integer(d) || D != as.integer(D) || s < 1) stop("d, D deben ser enteros no negativos y s positivo")
  z <- as.numeric(y)
  for (i in seq_len(d)) if (length(z)>1) z <- z[-1] - z[-length(z)]
  for (i in seq_len(D)) if (length(z)>s) z <- z[(s+1):length(z)] - z[1:(length(z)-s)]
  z
}
polinomio_rezago <- function(coef) {
  if (length(coef)<2 || coef[1] == 0) stop("Coeficientes inválidos")
  raices <- polyroot(coef)
  data.frame(raiz=raices, modulo=Mod(raices), real=Re(raices), imaginaria=Im(raices))
}
