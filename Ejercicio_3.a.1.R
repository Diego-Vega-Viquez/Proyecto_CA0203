library(dplyr)

curva_cero_cupon <- read.csv("datos/curvas-cero-cupon-csv.csv", sep = ";")

curva_cero_cupon <- curva_cero_cupon %>%
  mutate(
    Vencimiento_Años = as.numeric(sub(",", ".", Vencimiento..Años.)),
    Curva_Cero_Cupon_BCCR = as.numeric(sub(",", ".", Curva.Cero.Cupón.BCCR..Colones.))/100 
  )

bonos <- data.frame(
  Tipo = c("Cero Cupón", "Cero Cupón", "Cero Cupón", "Cuponado", "Cuponado", "Cuponado", "Cuponado"),
  Valor_Facial = c(10000000, 35000000, 80000000, 15000000, 20000000, 23000000, 40000000),
  Tasa_Cupon = c(0, 0, 0, 0.03, 0.02, 0.04, 0.07),
  Vencimiento = as.Date(c("2025-03-18", "2028-10-31", "2029-08-15", "2026-01-31", 
                          "2027-02-28", "2029-06-30", "2030-04-30")),
  Periodicidad = c(NA, NA, NA, 4, 12, 3, 2))

fecha_ref <- as.Date("2024-10-31")

bonos <- bonos %>%
  mutate(tau = as.numeric(difftime(Vencimiento, fecha_ref, units = "days")) / 365.25)

interpolar_tasa <- function(tau, curva) {
  idx_t1 <- max(which(curva$Vencimiento_Años <= tau))
  idx_t2 <- min(which(curva$Vencimiento_Años > tau))
  
  T1 <- curva$Vencimiento_Años[idx_t1]
  T2 <- curva$Vencimiento_Años[idx_t2]
  
  tasa_T1 <- curva$Curva_Cero_Cupon_BCCR[idx_t1]
  tasa_T2 <- curva$Curva_Cero_Cupon_BCCR[idx_t2]
  
  tasa_interpolada <- ((T2 - tau) / (T2 - T1)) * tasa_T1 + ((tau - T1) / (T2 - T1)) * tasa_T2
  
  return(tasa_interpolada)
}

bonos <- bonos %>%
  mutate(
    tasa_descuento = sapply(tau, function(t) interpolar_tasa(t, curva_cero_cupon))
  )

calcular_precio <- function(valor_facial, tasa_cupon, tasa_descuento, periodicidad, tau) {
  if (is.na(periodicidad)) {
    return(valor_facial / ((1 + tasa_descuento)^tau))
  } else {
    n <- floor(periodicidad * tau)
    flujos <- rep((valor_facial * tasa_cupon) / periodicidad, n)
    flujos[n] <- flujos[n] + valor_facial
    descuento <- (1 + tasa_descuento / periodicidad)^(1:n)
    return(sum(flujos / descuento))
  }
}

bonos$P <- mapply(calcular_precio, bonos$Valor_Facial, bonos$Tasa_Cupon, bonos$tasa_descuento, bonos$Periodicidad, bonos$tau)

calcular_duracion <- function(tau, P, tasa_descuento, valor_facial) {
  flujo_descuento <- valor_facial / ((1 + tasa_descuento)^tau)
  duracion_individual <- tau * flujo_descuento / P
  return(duracion_individual)
}

bonos$Duracion <- mapply(calcular_duracion, bonos$tau, bonos$P, bonos$tasa_descuento, bonos$Valor_Facial)

bonos$Duracion_Modificada <- bonos$Duracion / (1 + bonos$tasa_descuento)

calcular_convexidad <- function(tau, tasa_descuento, valor_facial, P) {
  flujo_descuento <- valor_facial / ((1 + tasa_descuento)^tau)
  convexidad_individual <- tau * (tau + 1) * flujo_descuento / (P * (1 + tasa_descuento)^2)
  return(convexidad_individual)
}

bonos$Convexidad <- mapply(calcular_convexidad, bonos$tau, bonos$tasa_descuento, bonos$Valor_Facial, bonos$P)

total_valor_presente <- sum(bonos$P)
Duracion_Portafolio <- sum((bonos$P / total_valor_presente) * bonos$Duracion)
Duracion_Modificada_Portafolio <- sum((bonos$P / total_valor_presente) * bonos$Duracion_Modificada)
Convexidad_Portafolio <- sum((bonos$P / total_valor_presente) * bonos$Convexidad)

print(bonos)
cat("Duración del Portafolio:", round(Duracion_Portafolio, 4), "\n")
cat("Duración Modificada del Portafolio:", round(Duracion_Modificada_Portafolio, 4), "\n")
cat("Convexidad del Portafolio:", round(Convexidad_Portafolio, 4), "\n")
