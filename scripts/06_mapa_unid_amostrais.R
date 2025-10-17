### fazer mapa das unidades amostrais e pontos de amostragem ###

# carregar pacotes
library(terra)
library(geobr)
library(readr)
library(sf)

#Shapes via geobr (dados vetoriais)
# baixar estado da Bahia
bahia <- geobr::read_state(code_state = "BA", year = 2020, simplified = TRUE)
bahia_v <- vect(bahia)  # converter para SpatVector

brasil <- read_country(year = 2020, simplified = TRUE)
brasil_v <- vect(brasil)

pontos <- read_csv("outputs/tables/pontos.csv")
alt_classificado <- rast("outputs/espacial/altitude_classificado.tif")

# transformar tabela em spatvector
pontos_vect <- vect(
  x = pontos, 
  geom = c("Longitude", "Latitude"), # As colunas X e Y do seu CSV
  crs = "epsg:4326"                 # O CRS que você usou (WGS 84 - Lat/Lon)
)

plot(altitude)
plot(bahia_v, add = T)
plot(pontos_vect, add = T)

# Transformar o SpatVector (pontos_vect) em um objeto sf
pontos_sf <- st_as_sf(pontos_vect)

# PREPARAÇÃO DOS DADOS

# Raster de Altitude (Converter para Data Frame)
altitude_df <- as.data.frame(alt_classificado, xy=TRUE, na.rm=TRUE)

# Renomeia a coluna de altitude (3ª posição) para 'altitude_class'
names(altitude_df)[3] <- "altitude_class"

# Pontos de Coleta (Converter para SF e fator)
# O objeto 'pontos_sf' já está carregado e possui a coluna 'Categoria'.
# Apenas garantimos que 'Categoria' seja um fator para a plotagem.
pontos_sf$Categoria <- factor(pontos_sf$Categoria)


# --- 3. DEFINIÇÃO DE ESTILOS ---

# Cores para os Pontos de Categoria (Manuais)
cores_pontos <- c("1" = "blue", "2" = "red", "3" = "green")
rotulos_pontos <- c(
  "1" = "Florestas de Terras Baixas (N=60)", 
  "2" = "Florestas Submontanas (N=56)", 
  "3" = "Florestas Montanas (N=24)"
)

# Cores para o Raster de Altitude (escala de cores contínua)
cores_altitude <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(unique(altitude_df$altitude_class)))

# CONSTRUÇÃO DO MAPA
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
  # Escala de cores para o Raster (Altitude)
  scale_fill_manual(
    name = "Altitude",
    values = cores_altitude, 
    guide = "none" # Desliga a legenda do raster para não poluir
  ) +
  
  # Escala de Cores para os pontos (Categorias)
  scale_color_manual(
    name = "Indivíduos amostrados",
    values = cores_pontos,
    labels = rotulos_pontos
  ) +
  scale_x_continuous(
    breaks = seq(-40, -39, by = 0.5), # Exemplo: De 0.5 em 0.5 graus
    labels = function(x) paste0(abs(x), "°W") # Formato como no seu mapa
  ) +
  
  # Eixo Y (Latitude): Reduz a frequência das grades
  scale_y_continuous(
    breaks = seq(-19, -13, by = 1), # Exemplo: De 1 em 1 grau (ou 0.5 se ainda for denso)
    labels = function(y) paste0(abs(y), "°S") # Formato como no seu mapa
  ) +
  
  # 4. TÍTULOS E APARÊNCIA
  labs(
    title = "Localização dos pontos de amostragem de Euterpe edulis",
    caption = "Fontes: TOPODATA e MapBiomas"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_blank(),
    # Linhas de grade mais suaves
    panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

mapa_final

# SALVAR O MAPA
ggsave("outputs/espacial/altitude_pontos_bahia_final.png", mapa_final, width = 12, height = 10, units = "in", dpi = 300)