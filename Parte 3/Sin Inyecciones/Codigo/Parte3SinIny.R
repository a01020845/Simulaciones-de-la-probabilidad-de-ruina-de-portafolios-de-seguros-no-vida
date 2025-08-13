set.seed(89)  # para reproducibilidad

### MODELO ###

# PARTE III: Evaluar estrategias de inyeccion
# Sin inyecciones

## Parametros generales del modelo
u <- 24 # capital inicial
c <- 1 # tasa de ingreso de primas por unidad de tiempo
Tmax <- 120 # tiempo total a simular
nsim <- 10000 # Numero de simulaciones para Monte Carlo

## Parametros de frecuencia
# Parametros de la distribucion de arribos de siniestros (Exponencial)
lambda <- 0.6

## Parametros de tamano
# Parametros de la distribucion de tamanos de siniestros (Exponencial)
lambda_2 <- 0.5

### Monte Carlo ###

## Condiciones iniciales
contador_ruinas <- 0 # Contador de ruinas
contador_no_ruinas <- 0 # Contador de no-ruinas
tiempos_ruina <- numeric() # Tiempos de ruinas
utilidades <- numeric(nsim) # Utilidades
graficado_ruina <- FALSE
graficado_no_ruina <- FALSE

## Simulaciones
for (i in 1:nsim) {
  
  ## Tiempos de arribo de los siniestros
  arribos_brutos_sin <- cumsum(rexp(n = Tmax * lambda * 2, rate = lambda))
  if (tail(arribos_brutos_sin, 1) < Tmax) {
    warning("No se generaron suficientes eventos para cubrir hasta Tmax. Aumentar n.")
  }
  arribos_sin <- arribos_brutos_sin[arribos_brutos_sin <= Tmax]
  n_sin <- length(arribos_sin)
  
  ## Tamanos de los siniestros
  siniestros <- rexp(n_sin, rate = lambda_2)
  
  ### PROCESO DE SUPERAVIT ###
  
  ## Malla temporal
  malla_temp <- seq(0, Tmax, by = 0.001)
  Ct <- numeric(length(malla_temp))
  sin_index <- 1
  acumulados_sin <- 0
  ruina_ocurrida <- FALSE
  tiempo_ruina <- NA
  
  for (j in seq_along(malla_temp)) {
    t <- malla_temp[j]
    
    ## Suma siniestros que ya ocurrieron hasta t
    while (sin_index <= n_sin && arribos_sin[sin_index] <= t) {
      acumulados_sin <- acumulados_sin + siniestros[sin_index]
      sin_index <- sin_index + 1
    }
    
    ## Calculamos el superavit
    Ct[j] <- u + c * t - acumulados_sin
    
    ## Verificamos si hubo ruina
    if (!ruina_ocurrida && Ct[j] < 0) {
      ruina_ocurrida <- TRUE
      tiempo_ruina <- t
      break  # podemos salir del bucle si ya hay ruina
    }
  }
  
  ## Calculamos las utilidades (si hubo)
  t_final <- if (ruina_ocurrida) tiempo_ruina else Tmax # Tiempo final efectivo
  prima_total <- c * t_final # Prima acumulada hasta ese momento
  utilidades[i] <- prima_total - acumulados_sin # Utilidad (no incluye inyecciones, solo primas - siniestros)
  
  ## Almacenamos los datos de ruina (si hubo) o no-ruina
  if (ruina_ocurrida) {
    contador_ruinas <- contador_ruinas + 1
    tiempos_ruina <- c(tiempos_ruina, tiempo_ruina)
  }
  if (!ruina_ocurrida) {
    contador_no_ruinas <- contador_no_ruinas + 1
  }
  
  ## Graficamos la primera no-ruina
  if (!ruina_ocurrida && !graficado_no_ruina) {
    plot(malla_temp, Ct, type = "l", lwd = 2, col = "blue",
         xlab = "Tiempo", ylab = "Superavit C(t)",
         main = paste0("Caminata Aleatoria - Simulacion ", i))
    abline(h = 0, col = "red", lty = 2)
    graficado_no_ruina <- TRUE
  }
  
  ## Graficamos la primera ruina
  if (ruina_ocurrida && !graficado_ruina) {
    plot(malla_temp, Ct, type = "l", lwd = 2, col = "blue",
         xlab = "Tiempo", ylab = "Superavit C(t)",
         main = paste0("Caminata Aleatoria - Simulacion ", i))
    abline(h = 0, col = "red", lty = 2)
    graficado_ruina <- TRUE
  }
}

### Resultados ###

## Calculamos los resultados de las simulaciones Monte Carlo
prob_ruina <- contador_ruinas / nsim
tiempo_prom_ruina <- ifelse(length(tiempos_ruina) > 0, mean(tiempos_ruina), NA)

## Calculamos el Intervalo de confianza
z <- 1.96  # valor z para 95%
se <- sqrt(prob_ruina * (1 - prob_ruina) / nsim)  # error estandar
ic_inf <- prob_ruina - z * se
ic_sup <- prob_ruina + z * se
# Nota: 90% z = 1.645, 95% z = 1.96, 99% z = 2.576

### Resumen ###

## Cuadro Resumen
cat("Probabilidad estimada de ruina:", round(prob_ruina, 4), "\n")
cat("Intervalo de confianza al 95%: [", round(ic_inf, 4), ",", round(ic_sup, 4), "]\n")
cat("Tiempo promedio hasta la ruina (condicionado):", round(tiempo_prom_ruina, 4), "\n")
cat("Utilidad promedio:", round(mean(utilidades), 4), "\n")

## Graficas Resumen
if (length(tiempos_ruina) > 0) {
  hist(tiempos_ruina, breaks = 50, col = "lightblue",
       main = "Distribucion de los tiempos de ruina",
       xlab = "Tiempo de ruina", ylab = "Frecuencia")
}

hist(utilidades, breaks = 50, col = "lightgreen",
     main = "Distribucion de la utilidad (prima - siniestros)",
     xlab = "Utilidad", ylab = "Frecuencia")
