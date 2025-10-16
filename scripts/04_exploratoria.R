# objetivo: verificar a influencia de fatores abioticos em atributos foliares em diferentes altitudes
# OK criar médias por unidades amostrais (grafico de umas duas variáveis)
# OK criar as latitudes e longitudes
# extrair altitude e adicionar na tabela
# Fazer uma regressão altitude e uma resposta
# Fazer mapa das unidades amostrais (9),
# colorir os 20 pontos de cada unidade de acordo com a altitude

### exploratorias dos dados ###

# pacotes necessarios
library(readr)
library(ggplot2)
library(tibble)
library(tidyverse)
library(patchwork)

#carregar planilhas
dados_completos <- read_csv("data/data_tratados/dados_completos.csv")
dados_completos

# excluir coluna 1, que apenas numera as linhas
dados_completos <- dados_completos %>%
  select(-1)

# Calcular média e desvio padrão por unidade_amostral
resumo_por_unidade <- dados_completos %>%
  group_by(unidade_amostral) %>%
  summarise(
    # Aplica a função de média e desvio padrão a todas as colunas que são numéricas
    across(.cols = where(is.numeric),
           .fns = list(Media = mean, DP = sd),
           .names = "{.col}_{.fn}") # Padrão de nomeação das novas colunas: {nome_original}_Media e {nome_original}_DP
  ) %>%
  # Desagrupa
  ungroup()

# Visualizar os resultados
print(resumo_por_unidade)

## graficos de boxplot de PIabs e PItotal por unidade amostral

# Verificar a estrutura da coluna 'unidade_amostral'
str(dados_completos$unidade_amostral)
# transformar unidades amostrais em fator
dados_completos$unidade_amostral <- 
  as.factor(dados_completos$unidade_amostral)

# Boxplot para piabs
plot_piabs <- ggplot(dados_completos, aes(x = unidade_amostral, y = piabs)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") + # Cria o boxplot
  labs(
    title = expression(PI[abs] ~ "por unidade amostral"),
    x = "Unidade Amostral",
    y = expression(PI[abs] ~ "(Índice de desempenho)")
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) # Centraliza o título
plot_piabs

# Boxplot para pitotal
plot_pitotal <- ggplot(dados_completos, aes(x = unidade_amostral, y = pitotal)) +
  geom_boxplot(fill = "lightcoral", color = "darkred") + # Cria o boxplot com cores diferentes
  labs(
    title = expression(PI[total] ~ "por unidade amostral"),
    x = "Unidade Amostral",
    y = expression(PI[total] ~ "(Índice de desempenho total)")
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# 4. Exibir os gráficos
print(plot_piabs)
print(plot_pitotal)

# Para exibir os dois gráficos lado a lado (requer o pacote 'patchwork')
plots <- plot_piabs + plot_pitotal


# salvando produtos
write.csv(resumo_por_unidade, "outputs/tables/medias_unidade_amostral.csv", 
          row.names = FALSE)
ggsave("outputs/figures/boxplot_pi.png", plot = plots, width = 9, height = 5, dpi = 300)
