set.seed(89)  # para reproducibilidad

### DISTRIBUCIONES ###

# PARTE IV: Sensibilidad al horizonte temporal

### Fijos ###

## Parametros de frecuencia
# Parametros de la distribucion de arribos de siniestros (Exponencial)
lambda <- 0.6
# Parametros de la distribucion de arribos de inyecciones (Exponencial)
lambda_2 <- 0.3

## Parametros de tamano
# Parametros de la distribucion de tamanos de siniestros (Exponencial)
lambda_3 <- 0.5
# Parametros de la distribucion de tamanos de inyecciones (Gamma)
alpha <- 2
sigma <- 0.6

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
       x = "Tiempo entre eventos", y = "Densidad",
       color = "Distribucion") +
  theme_minimal()

# Tamanos Fijos
x_tamano <- seq(0, 15, length.out = 1000)

df_tamanos <- data.frame(
  x = rep(x_tamano, 2),
  densidad = c(
    dexp(x_tamano, rate = lambda_3),
    dgamma(x_tamano, shape = alpha, scale = sigma)
  ),
  tipo = rep(c("Siniestros: Exponencial (lambda = 0.5)", 
               "Inyecciones: Gamma (alpha = 2, sigma = 0.6)"), each = length(x_tamano))
)

ggplot(df_tamanos, aes(x = x, y = densidad, color = tipo)) +
  geom_line(size = 1) +
  labs(title = "Tamano de Siniestros e Inyecciones",
       x = "Tamano", y = "Densidad",
       color = "Distribucion") +
  theme_minimal()
