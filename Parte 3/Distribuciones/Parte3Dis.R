set.seed(89)  # para reproducibilidad

### DISTRIBUCIONES ###

# PARTE III: Evaluar estrategias de inyeccion

### Fijos ###

## Parametros de frecuencia
# Parametros de la distribucion de arribos de siniestros (Exponencial)
lambda <- 0.6
# Parametros de la distribucion de arribos de inyecciones (Exponencial)
lambda_2 <- 0.3

## Parametros de tamano
# Parametros de la distribucion de tamanos de siniestros (Exponencial)
lambda_3 <- 0.5

### Cambiantes ###

## Parametros de tamano
# Parametros de la distribucion de tamanos de inyecciones (Gamma)
alpha <- 2
sigma <- 0.6
# Parametros de la distribucion de tamanos de inyecciones (Pareto)
xm <- 1
alpha <- 6

### GRAFICAS ###

library(ggplot2)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Frecuencias
x_freq <- seq(0, 20, length.out = 1000)
df_freq <- data.frame(
  x = rep(x_freq, 2),
  densidad = c(dexp(x_freq, rate = lambda),
               dexp(x_freq, rate = lambda_2)),
  tipo = rep(c("Siniestros (lambda = 0.6)", "Inyecciones (lambda = 0.3)"), each = length(x_freq))
)

ggplot(df_freq, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Distribuciones de Frecuencia (Exponencial)",
    x = "Tiempo entre eventos", y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()

# Tamanos Fijos
x_fijo <- seq(0, 15, length.out = 1000)
df_fijo <- data.frame(
  x = x_fijo,
  densidad = dexp(x_fijo, rate = lambda_3),
  tipo = "Exponencial (lambda = 0.5)"
)

ggplot(df_fijo, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Tamano de Siniestros (Distribucion Exponencial)",
    x = "Tamano", y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()

# Tamanos Cambiantes
if (!require(EnvStats)) install.packages("EnvStats")
library(EnvStats)

x_camb <- seq(0.01, 15, length.out = 1000)
df_camb <- data.frame(
  x = rep(x_camb, 2),
  densidad = c(
    dgamma(x_camb, shape = 2, scale = 0.6),
    dpareto(x_camb, location = 1, shape = 6)
  ),
  tipo = rep(c(
    "Gamma (alpha = 2, sigma = 0.6)",
    "Pareto (xm = 1, alpha = 6)"
  ), each = length(x_camb))
)

ggplot(df_camb, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Tamano de Inyecciones (Distribuciones Cambiantes)",
    x = "Tamano", y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()
