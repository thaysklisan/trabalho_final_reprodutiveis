### verificar influencia de variaveis abioticas em atributos morfofisiologicos ###

# carregar pacotes
library(readr)
library(ggplot2)
library(tibble)
library(tidyverse)
library(lmerTest)

# 1. Carregar e Preparar os Dados
dados <- read_csv("data/data_tratados/dados_completos.csv")

dados <- dados %>%
  mutate(
    # Sem transformação logarítmica - usando LMA na escala original
    formacao = as.factor(formacao),
    campanha = as.factor(campanha)
  )

# 3. Ajuste do Modelo de Regressão Linear Múltipla com LMA
# Fórmula: lma ~ Efeitos Fixos
modelo_ols_original <- lm(
  lma ~ formacao + campanha + temperatura + umidade + precipitacao,
  data = dados
)

# 4. Análise dos Resultados
# 4.1. Sumário Completo do Modelo
summary(modelo_ols_original)

# 4.2. Teste de Significância Global dos Fatores
Anova(modelo_ols_original, type = "III") 

# 5. DIAGNÓSTICO CRÍTICO: VERIFICAÇÃO DE PRESSUPOSTOS
# Execute estes plots e verifique:
# - Topo Esquerdo (Residuals vs Fitted): O padrão deve ser uma "nuvem" aleatória. Se for um "cone" (funil) ou curva, há violação.
# - Topo Direito (Normal Q-Q): Os pontos devem seguir a linha reta. Se desviarem nas pontas, há violação.
par(mfrow = c(2, 2)) 
plot(modelo_ols_original)
par(mfrow = c(1, 1))
