### verificar influencia de variaveis abioticas em atributos morfofisiologicos ###

# carregar pacotes
library(readr)
library(ggplot2)
library(tibble)
library(tidyverse)

# 1. Carregar e Preparar os Dados
dados <- read_csv("data/data_tratados/dados_completos.csv")

dados <- dados %>%
  mutate(
    # Sem transformação logarítmica - usando LMA na escala original
    formacao = as.factor(formacao),
    campanha = as.factor(campanha)
  )

# Ajuste do Modelo de Regressão Linear Múltipla com LMA
# Fórmula: lma ~ Efeitos Fixos
modelo <- lm(
  lma ~ formacao + campanha + temperatura + umidade + precipitacao,
  data = dados
)

# Análise dos Resultados
# Sumário Completo do Modelo
summary(modelo)

# Teste de Significância Global dos Fatores
aov(modelo) 

# 5. DIAGNÓSTICO CRÍTICO: VERIFICAÇÃO DE PRESSUPOSTOS
par(mfrow = c(2, 2)) 
plot(modelo)
par(mfrow = c(1, 1))
