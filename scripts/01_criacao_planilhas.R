### criando planilha de dados ###

# carregar pacotes
library(readr)
library(dplyr)
library(tibble)
library(tidyverse)
library(lubridate)

# importar planiha vazia de atributos
dados <- read.csv2("data/data_raw/dados_atributos.csv")
dados <- as_tibble(dados)
head(dados)


# criar variaveis categoricas formacao, unidade_amostral, campanha, individuos e codigos
dados <- tibble(formacao = c("baixa", "submontana", "montana")) %>%
  # associa as unidades a cada formação
  mutate(unidade_amostral = case_when(
    formacao == "baixa" ~ list(c("a", "b", "c")),
    formacao == "submontana" ~ list(c("d", "e", "f")),
    formacao == "montana" ~ list(c("g", "h", "i"))
  )) %>%
  unnest(unidade_amostral) %>%
  crossing(campanha = 1:2) %>%
  mutate(mes_ano = case_when(
    campanha == 1 ~ "abr/2026",
    campanha == 2 ~ "nov/2026"
  )) %>%
  # gera 20 indivíduos diretamente com map_dfr
  mutate(dados_individuos = map(unidade_amostral, ~ tibble(
    id_individuo = paste0(.x, sprintf("%02d", 1:20))
  ))) %>%
  unnest(dados_individuos) %>%
  mutate(
    codigo = paste0(substr(formacao, 1, 1), campanha, unidade_amostral)
  )

dados

# Criando valores numericos fictícios
set.seed(123)  # fixar os mesmos valores

# criar valores fictícios para os atributos foliares
dados <- dados %>%
  mutate(
    h = round(runif(n(), min = 20,  max = 50), 1), # altura em cm (20 a 100)
    d = round(runif(n(), min = 1, max = 5), 2),   # diâmetro em mm (1 a 5)
    la = round(runif(n(), min = 30,  max = 110), 1), # área foliar em cm² (30 a 110)
    lma = round(runif(n(), min = 50,  max = 100), 1), # massa foliar especifica g/m² (50 a 100)
    awc = round(runif(n(), min = 60,  max = 90), 1), # conteudo absoluto de agua % (60 a 90)
    ldmc = round(runif(n(), min = 0.2, max = 0.6), 2), # conteúdo matéria seca (0.2 a 0.6 g/g)
    piabs = round(runif(n(), min = 0.5, max = 4), 2), # índice desempenho inicial (0.5 a 4)
    pitotal = round(runif(n(), min = 0.5,   max = 2), 2) # índice desempenho total (1 a 2)
  )

head(dados)


# importar planiha vazia de variaveis abioticas
dados2 <- read.csv2("data/data_raw/dados_abioticos.csv")
dados2 <- as_tibble(dados2)
head(dados2)

# Definir meses/anos para as campanhas
meses_anos <- c("abr/2026", "mai/2026", "jun/2026", "jul/2026",
                "ago/2026", "set/2026", "out/2026", "nov/2026",
                "dez/2026", "jan/2027", "fev/2027", "mar/2027", "abr/2027")

# criar valores fictícios para os variaveis abioticas
set.seed(123)
# Criar dados abióticos
dados2 <- tibble(
    formacao = c("baixa", "submontana", "montana")
  ) %>%
  mutate(unidade_amostral = case_when(
    formacao == "baixa" ~ list(c("a", "b", "c")),
    formacao == "submontana" ~ list(c("d", "e", "f")),
    formacao == "montana" ~ list(c("g", "h", "i"))
  )) %>%
  unnest(unidade_amostral) %>%
  crossing(mes_ano = meses_anos) %>%
  # gerar valores abióticos fictícios
  mutate(
    temperatura = round(case_when(
      formacao == "baixa" ~ rnorm(n(), 25, 1.5),
      formacao == "submontana" ~ rnorm(n(), 22, 1.2),
      formacao == "montana" ~ rnorm(n(), 10, 1)
    ), 1),
    umidade = round(case_when(
      formacao == "baixa" ~ rnorm(n(), 83, 3),
      formacao == "submontana" ~ rnorm(n(), 87, 2.5),
      formacao == "montana" ~ rnorm(n(), 92, 2)
    ), 1),
    dpv = round(case_when(
      formacao == "baixa" ~ rnorm(n(), 1.5, 0.2),
      formacao == "submontana" ~ rnorm(n(), 1, 0.15),
      formacao == "montana" ~ rnorm(n(), 0.5, 0.1)
    ), 2),
    precipitacao = round(case_when(
      formacao == "baixa" ~ rnorm(n(), 150, 15),
      formacao == "submontana" ~ rnorm(n(), 150, 20),
      formacao == "montana" ~ rnorm(n(), 150, 25)
    ), 1)
  ) %>%
  mutate(
    solo = case_when(
      formacao == "baixa" ~ "A",
      formacao == "submontana" ~ "B",
      formacao == "montana" ~ "C"
    )
  )

dados2

# Selecionar apenas os meses de interesse em dados2
dados2_sel <- dados2 %>%
  filter(mes_ano %in% c("abr/2026", "nov/2026"))

# Fazer o join
dados_completos <- dados %>%
  left_join(dados2_sel, by = c("formacao", "unidade_amostral", "mes_ano"))



# salvar planilhas com dados ficticios
write.csv(dados, "data/data_raw/atributos_ficticios.csv")
write.csv(dados2, "data/data_raw/abioticos_ficticios.csv")
write.csv(dados_completos, "data/data_tratados/dados_completos.csv")
