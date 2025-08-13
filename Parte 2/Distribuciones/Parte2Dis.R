set.seed(89)  # para reproducibilidad

### DISTRIBUCIONES ###

# PARTE II: Explorar diferentes procesos de frecuencia

### Fijos ###

## Parametros de frecuencia
# Parametros de la distribucion de arribos de inyecciones (Exponencial)
lambda_2 <- 0.3

## Parametros de tamano
# Parametros de la distribucion de tamanos de siniestros (Exponencial)
lambda_3 <- 0.5
# Parametros de la distribucion de tamanos de inyecciones (Gamma)
alpha <- 2
sigma <- 0.6

### Cambiantes ###

## Parametros de frecuencia
# Parametros de la distribucion de arribos de siniestros (Exponencial)
lambda <- 0.6
# Parametros de la distribucion de arribos de siniestros (Gamma)
alpha <- 2
sigma <- 0.835
# Parametros de la distribucion de arribos de siniestros (Log-normal)
mu <- 0.011
sigma <- 1
# Parametros de la distribucion de arribos de siniestros (Weibull)
sigma <- 1.85
k <- 1.5

### GRAFICAS ###

library(ggplot2)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Frecuencias Cambiantes
x_freq <- seq(0.01, 20, length.out = 1000)

df_freq <- data.frame(
  x = rep(x_freq, 4),
  densidad = c(
    dexp(x_freq, rate = 0.6),
    dgamma(x_freq, shape = 2, scale = 0.835),
    dlnorm(x_freq, meanlog = 0.011, sdlog = 1),
    dweibull(x_freq, shape = 1.5, scale = 1.85)
  ),
  tipo = rep(c(
    "Exponencial (lambda = 0.6)",
    "Gamma (alpha = 2, sigma = 0.835)",
    "Log-normal (mu = 0.011, sigma = 1)",
    "Weibull (k = 1.5, sigma = 1.85)"
  ), each = length(x_freq))
)

ggplot(df_freq, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Distribuciones de Frecuencia de Siniestros (Distribuciones Cambiantes)",
    x = "Tiempo entre siniestros",
    y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()

# Tamanos Fijos
x_tamano_sin <- seq(0, 15, length.out = 1000)
df_tamano_sin <- data.frame(
  x = x_tamano_sin,
  densidad = dexp(x_tamano_sin, rate = 0.5),
  tipo = "Exponencial (lambda = 0.5)"
)

ggplot(df_tamano_sin, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Tamano de Siniestros (Distribucion Exponencial)",
    x = "Tamano",
    y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()

x_iny <- seq(0, 10, length.out = 1000)
df_gamma_iny <- data.frame(
  x = x_iny,
  densidad = dgamma(x_iny, shape = 2, scale = 0.6),
  tipo = "Gamma (alpha = 2, sigma = 0.6)"
)

ggplot(df_gamma_iny, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(
    title = "Tamano de Inyecciones (Distribucion Gamma)",
    x = "Tamano",
    y = "Densidad",
    color = "Distribucion"
  ) +
  theme_minimal()
