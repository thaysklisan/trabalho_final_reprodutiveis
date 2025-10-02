### criando planilha de dados ###

# carregar pacotes
library(readr)
library(dplyr)
library(tibble)
library(lubridate)

# importar planiha vazia de atributos
dados <- read.csv2("data/data_raw/dados_atributos.csv")
dados <- as_tibble(dados)
head(dados)

# Criando valores fictícios
set.seed(123)  # fixar os mesmos valores

# criar valores fictícios para os atributos foliares e excluir lat e long
dados <- dados %>%
select(-lat, -long) %>%
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

# criar valores fictícios para os variaveis abioticas e excluir lat e long
set.seed(123)
dados2 <- dados2 %>%
  select(-lat, -long) %>%
  mutate(
    temperatura_mensal = round(runif(n(), 20, 30), 1), # temperatura em °C
    rh_mensal = round(runif(n(), 60, 100), 1),   # umidade relativa %
    dpv_mensal = round(runif(n(), 0.01, 0.9), 1), # deficit de pressao de vapor kPa
    abertura_dossel = round(runif(n(), 65, 95), 1)
)

head(dados2)

# adicionar "01/" no início para criar formato "01/abr/26"
dados2 <- dados2 %>%
  mutate(mes = dmy(paste0("01/", mes)))

# salvar planilhas com dados ficticios
write.csv(dados, "data/data_raw/atributos_ficticios.csv")
write.csv(dados2, "data/data_raw/abioticos_ficticios.csv")
