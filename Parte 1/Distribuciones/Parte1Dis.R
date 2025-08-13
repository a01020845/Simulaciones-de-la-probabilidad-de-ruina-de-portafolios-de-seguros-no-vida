set.seed(89)  # para reproducibilidad

### DISTRIBUCIONES ###

# PARTE I: Explorar distribuciones de tamano de siniestros

### Fijos ###

## Parametros de frecuencia
# Parametros de la distribucion de arribos de siniestros (Exponencial)
lambda <- 0.6
# Parametros de la distribucion de arribos de inyecciones (Exponencial)
lambda_2 <- 0.3

## Parametros de tamano
# Parametros de la distribucion de tamanos de inyecciones (Gamma)
alpha <- 2
sigma <- 0.6

### Cambiantes ###

## Parametros de tamano
# Parametros de la distribucion de tamanos de siniestros (Exponencial)
lambda_3 <- 0.5
# Parametros de la distribucion de tamanos de siniestros (Gamma)
alpha <- 2
sigma <- 1
# Parametros de la distribucion de tamanos de siniestros (Log-normal)
mu <- -0.3069
sigma <- sqrt(2)
# Parametros de la distribucion de tamanos de siniestros (Pareto)
xm <- 1.2
alpha <- 2.5

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
  labs(title = "Distribuciones de Frecuencia (Exponencial)",
       x = "Tiempo entre eventos", y = "Densidad") +
  theme_minimal()

# Tamanos Fijos
x_gamma_fijo <- seq(0, 10, length.out = 1000)
df_gamma_fijo <- data.frame(
  x = x_gamma_fijo,
  densidad = dgamma(x_gamma_fijo, shape = 2, scale = 0.6),
  tipo = "Gamma (alpha = 2, sigma = 0.6)"
)

ggplot(df_gamma_fijo, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Tamano de Inyecciones (Distribucion Gamma)",
    x = "Tamano",
    y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()

# Tamanos Cambiantes
if (!require(EnvStats)) install.packages("EnvStats")
library(EnvStats)

x_tamanos <- seq(0.01, 15, length.out = 1000)
df_tamanos <- data.frame(
  x = rep(x_tamanos, 4),
  densidad = c(
    dexp(x_tamanos, rate = 0.5),
    dgamma(x_tamanos, shape = 2, scale = 1),
    dlnorm(x_tamanos, meanlog = -0.3069, sdlog = sqrt(2)),
    dpareto(x_tamanos, location = 1.2, shape = 2.5)
  ),
  tipo = rep(c(
    "Exponencial (lambda = 0.5)",
    "Gamma (alpha = 2, sigma = 1)",
    "Log-normal (mu = -0.3069, sigma = sqrt(2))",
    "Pareto (xm = 1.2, alpha = 2.5)"
  ), each = length(x_tamanos))
)

ggplot(df_tamanos, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Tamano de Siniestros (Distribuciones Cambiantes)",
    x = "Tamano",
    y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()