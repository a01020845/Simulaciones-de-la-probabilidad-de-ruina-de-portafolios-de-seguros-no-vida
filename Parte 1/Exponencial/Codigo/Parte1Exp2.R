set.seed(89) # para reproducibilidad

### MODELO ###

# PARTE I: Explorar distribuciones de tamano de siniestros
# Exponencial

## Crear funcion para simular combinaciones
simular_resultado <- function(u, c, Tmax = 120, nsim = 250,
                              lambda = 0.6, lambda_2 = 0.3,
                              lambda_3 = 0.5, alpha = 2,
                              sigma = 0.6) {
  
  ## Condiciones iniciales
  contador_ruinas <- 0
  tiempos_ruina <- numeric()
  utilidades <- numeric(nsim)
  
  ## Simulaciones
  for (i in 1:nsim) {
    
    ## Tiempos de arribo de los siniestros
    arribos_brutos_sin <- cumsum(rexp(n = Tmax * lambda * 2, rate = lambda))
    if (tail(arribos_brutos_sin, 1) < Tmax) {
      warning("No se generaron suficientes eventos para cubrir hasta Tmax. Aumentar n.")
    }
    arribos_sin <- arribos_brutos_sin[arribos_brutos_sin <= Tmax]
    n_sin <- length(arribos_sin)
    
    ## Tiempos de arribo de las inyecciones
    arribos_brutos_iny <- cumsum(rexp(n = Tmax * lambda_2 * 2, rate = lambda_2))
    if (tail(arribos_brutos_iny, 1) < Tmax) {
      warning("No se generaron suficientes eventos para cubrir hasta Tmax. Aumentar n.")
    }
    arribos_iny <- arribos_brutos_iny[arribos_brutos_iny <= Tmax]
    n_iny <- length(arribos_iny)
    
    ## Tamanos de los siniestros
    siniestros <- rexp(n_sin, rate = lambda_3)
    
    ## Tamanos de las inyecciones
    inyecciones <- rgamma(n_iny, shape = alpha, scale = sigma)
    
    ### PROCESO DE SUPERAVIT ###
    
    ## Malla temporal
    malla_temp <- seq(0, Tmax, by = 0.001)
    Ct <- numeric(length(malla_temp))
    sin_index <- 1
    acumulados_sin <- 0
    iny_index <- 1
    acumulados_iny <- 0
    ruina_ocurrida <- FALSE
    tiempo_ruina <- NA
    
    for (j in seq_along(malla_temp)) {
      t <- malla_temp[j]
      
      ## Suma siniestros que ya ocurrieron hasta t
      while (sin_index <= n_sin && arribos_sin[sin_index] <= t) {
        acumulados_sin <- acumulados_sin + siniestros[sin_index]
        sin_index <- sin_index + 1
      }
      
      ## Suma inyecciones que ya ocurrieron hasta t
      while (iny_index <= n_iny && arribos_iny[iny_index] <= t) {
        acumulados_iny <- acumulados_iny + inyecciones[iny_index]
        iny_index <- iny_index + 1
      }
      
      ## Calculamos el superavit
      Ct[j] <- u + c * t - acumulados_sin + acumulados_iny
      
      ## Verificamos si hubo ruina
      if (!ruina_ocurrida && Ct[j] < 0) {
        ruina_ocurrida <- TRUE
        tiempo_ruina <- t
        break
      }
    }
    
    ## Calculamos las utilidades (si hubo)
    t_final <- if (ruina_ocurrida) tiempo_ruina else Tmax
    prima_total <- c * t_final
    utilidades[i] <- prima_total - acumulados_sin
    
    ## Almacenamos los datos de ruina (si hubo)
    if (ruina_ocurrida) {
      contador_ruinas <- contador_ruinas + 1
      tiempos_ruina <- c(tiempos_ruina, tiempo_ruina)
    }
  }
  
  ### Resultados ###
  
  ## Resultados de las simulaciones Monte Carlo
  prob_ruina <- contador_ruinas / nsim
  tiempo_prom_ruina <- ifelse(length(tiempos_ruina) > 0, mean(tiempos_ruina), NA)
  utilidad_prom <- mean(utilidades)
  
  return(list(prob_ruina = prob_ruina,
              utilidad_prom = utilidad_prom,
              tiempo_prom_ruina = tiempo_prom_ruina))
}

### Simulaciones 2 ###

## Definir la rejilla
valores_u <- seq(12, 36, by = 1)
valores_c <- seq(0.5, 3, by = 0.25)
rejilla <- expand.grid(u = valores_u, c = valores_c)
rejilla$prob_ruina <- NA
rejilla$utilidad_prom <- NA

## Correr simulaciones sobre cada combinacion
for (i in 1:nrow(rejilla)) {
  u_val <- rejilla$u[i]
  c_val <- rejilla$c[i]
  resultado <- simular_resultado(u_val, c_val)
  rejilla$prob_ruina[i] <- resultado$prob_ruina
  rejilla$utilidad_prom[i] <- resultado$utilidad_prom
}

## Parametros aceptables
limite_ruina <- 0.01
utilidad_minima <- 18

## Filtrar resultados factibles
factibles <- subset(rejilla, prob_ruina <= limite_ruina & utilidad_prom >= utilidad_minima)
optima <- factibles[which.min(factibles$u + factibles$c), ]

### Resumen ###

## Cuadro resumen
print(optima)

## Graficas resumen
library(ggplot2)
ggplot(factibles, aes(x = u, y = c, fill = utilidad_prom)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Combinaciones viables (ruina <= 1 por ciento, utilidad >= 18)",
       x = "u (capital inicial)", y = "c (tasa de primas)")

# Exportar la tabla rejilla a CSV
write.csv(rejilla, file = "C:/Users/DiDi/Documents/JIM/Tesis/Simulaciones/Parte 1/Exponencial/Outputs/P1_Rej_Exp.csv", row.names = FALSE)
