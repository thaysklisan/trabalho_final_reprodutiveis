### fazer mapa das unidades amostrais e pontos de amostragem ###

# carregar pacotes
library(terra)
library(geobr)
library(readr)

#Shapes via geobr (dados vetoriais)
# baixar estado da Bahia
bahia <- geobr::read_state(code_state = "BA", year = 2020, simplified = TRUE)
bahia_v <- vect(bahia)  # converter para SpatVector

brasil <- read_country(year = 2020, simplified = TRUE)
brasil_v <- vect(brasil)

shape_ma <- vect ("data/data_raw/mata_atlantica/dominio_WGS84_forest.shp")
plot(shape_ma)

pontos <- read_csv("outputs/tables/pontos.csv")
altitude <- rast("outputs/espacial/altitude_mosaico.tif")

# transformar tabela em spatvector
pontos_vect <- vect(
  x = pontos, 
  geom = c("Longitude", "Latitude"), # As colunas X e Y do seu CSV
  crs = "epsg:4326"                 # O CRS que você usou (WGS 84 - Lat/Lon)
)

plot(altitude)
plot(bahia_v, add = T)
plot(pontos_vect, add = T)




# 1. Instalação e Carregamento de Pacotes
library(terra)
library(sf)
library(ggplot2)
library(RColorBrewer) 

# --- 2. PREPARAÇÃO DOS DADOS ---

# A. Raster de Altitude (Converter para Data Frame)
altitude_df <- as.data.frame(altitude_floresta, xy=TRUE, na.rm=TRUE)

# Renomeia a coluna de altitude (3ª posição) para 'altitude_class'
names(altitude_df)[3] <- "altitude_class"

# B. Pontos de Coleta (Converter para SF e fator)
# O objeto 'pontos_sf' já está carregado e possui a coluna 'Categoria'.
# Apenas garantimos que 'Categoria' seja um fator para a plotagem.
pontos_sf$Categoria <- factor(pontos_sf$Categoria)


# --- 3. DEFINIÇÃO DE ESTILOS ---

# Cores para os Pontos de Categoria (Manuais)
cores_pontos <- c("1" = "blue", "2" = "red", "3" = "green")
rotulos_pontos <- c(
  "1" = "Categoria 1 (N=60)", 
  "2" = "Categoria 2 (N=56)", 
  "3" = "Categoria 3 (N=24)"
)

# Cores para o Raster de Altitude (escala de cores contínua)
cores_altitude <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(unique(altitude_df$altitude_class)))


# --- 4. CONSTRUÇÃO DO MAPA (ggplot2) ---

mapa_final <- ggplot() +
  
  # 1. CAMADA RASTER (BASE)
  geom_raster(data = altitude_df, aes(x = x, y = y, fill = factor(altitude_class))) +
  
  # 2. PONTOS DE COLETA
  geom_sf(data = pontos_sf, 
          aes(color = Categoria), # CORRIGIDO: Usando a coluna 'Categoria'
          size = 2.5, 
          shape = 21, 
          stroke = 0.5) +
  
  # 3. ESCALAS E LEGENDAS
  # Escala de Cores para o Raster (Altitude)
  scale_fill_manual(
    name = "Altitude (Classes)",
    values = cores_altitude, 
    guide = "none" # Desliga a legenda do raster para não poluir
  ) +
  
  # Escala de Cores para os Pontos (Categorias)
  scale_color_manual(
    name = "Categorias de Amostragem",
    values = cores_pontos,
    labels = rotulos_pontos
  ) +
  
  # 4. TÍTULOS E APARÊNCIA
  labs(
    title = "Localização dos Pontos de Amostragem Estratificada na Mata Atlântica (BA)",
    subtitle = "Amostragem em áreas florestadas, colorida por estrato de altitude",
    caption = "Fontes: TOPODATA (MDE), MapBiomas (Fragmentos)"
  ) +
  
  theme_minimal() + 
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_blank()
  )

# 5. SALVAR O MAPA EM ALTA RESOLUÇÃO
caminho_saida_mapa <- "outputs/espacial/altitude_pontos_bahia_final.png"
ggsave(caminho_saida_mapa, mapa_final, width = 12, height = 10, units = "in", dpi = 300)

# Certifique-se de estar na pasta raiz do projeto R antes de executar!

# 1. Definir os arquivos/pastas grandes que CAUSARAM O ERRO DE 100MB
# Estes são os arquivos que precisam ser removidos do rastreamento do Git.
arquivos_grandes_remocao <- c(
  "outputs/espacial/altitude_mosaico.tif",
  "outputs/espacial/mapa_altitude_pontos_bahia_final.png",
  "data/data_raw/mata_atlantica/fragment_forest_area.tif",
  "data/data_raw/mata_atlantica/003_atlantic_spatial_forest_vegetation_binary.tif"
)

# 2. Remover as referências dos arquivos grandes do histórico do Git
# O comando 'git rm --cached' remove o arquivo do repositório Git, 
# mas o mantém no seu disco local.
for (arquivo in arquivos_grandes_remocao) {
  comando_git <- paste0("git rm --cached ", shQuote(arquivo))
  message(paste("Executando:", comando_git))
  # Executa o comando e captura o resultado
  resultado <- system(comando_git) 
  if (resultado != 0) {
    message(paste("AVISO: Falha ao remover", arquivo, "do Git (Pode não estar em cache)."))
  }
}

# 3. Adicionar Pastas de Dados Brutos e Outputs ao .gitignore
# Isso impede que o Git rastreie NOVOS arquivos grandes nessas pastas.

linhas_gitignore <- c(
  # Ignora todos os arquivos TIFF na pasta de dados brutos
  "data/data_raw/MDE_TOPODATA/*.tif", 
  # Ignora toda a pasta 'espacial' em outputs (pois contém o mosaico e outros outputs grandes)
  "outputs/espacial/",
  # Ignorar arquivos grandes em geral
  "*.tif",
  "*.gpkg",
  "*.csv"
)

# Abre o arquivo .gitignore (ou cria se não existir) e adiciona as linhas
write(
  linhas_gitignore,
  file = ".gitignore",
  append = TRUE # Adiciona ao final do arquivo existente
)