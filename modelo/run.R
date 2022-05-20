#Esto es un modelo basado en https://www.recetas-electorales.com/ajiaco.html pero a partir de mi propio promedio normalizado

library(tidyverse)
library(rstan)
library(lubridate)

# Rescato el promedio normalizado eliminando indecisos
modelo_encuestas <- read_csv("modelo/promedio_norm.csv") %>%
  mutate(promedio_ponderado_t = promedio)

# Lo monto para definir los priors

modelo_priors <- tibble(
  gp = modelo_encuestas$promedio_ponderado_t[2],
  fg = modelo_encuestas$promedio_ponderado_t[1],
  sf = modelo_encuestas$promedio_ponderado_t[5],
  rh = modelo_encuestas$promedio_ponderado_t[4],
  ib = modelo_encuestas$promedio_ponderado_t[3]) %>%
  dplyr::mutate(rest = 100-rowSums(across(where(is.numeric)))) %>%
  dplyr::mutate(across(everything(),~./100))

# Simulacion numero de encuestas 
modelo_n_encuestas <- 1e2
modelo_ensayos <- 1e5

# Simulacion de resultados de encuestas
modelo_multinomial <- rmultinom(modelo_ensayos, modelo_n_encuestas,modelo_priors) %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::summarise(across(everything(),median))

options(mc.cores = parallel::detectCores())

# Datos ####
modelo_data <- list(
  modelo_n_encuestas = modelo_n_encuestas,
  modelo_priors = c(modelo_multinomial$gp,
                    modelo_multinomial$fg,
                    modelo_multinomial$sf,
                    modelo_multinomial$rh,
                    modelo_multinomial$ib,
                    modelo_multinomial$rest)
)

# Estimacion ####
modelo_fit <- stan(file = "modelo/model.stan",
                   data = modelo_data,
                   control=list(adapt_delta=0.95),
                   iter=1e4,
                   chains=4,
                   cores=4,
                   seed=332211)