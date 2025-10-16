### gerar pontos de amostragem dentro em áreas florestadas nas 3 categorias de altitude que criei no script anterior ###

# carregar pacotes
library(terra)
library(dplyr)

## carregando dados arquivos necessarios
altitude <- rast ("outputs/espacial/altitude_mosaico.tif")
alt_classificado <- rast("outputs/espacial/altitude_classificado.tif")
ma_fragmentos <- rast("data/data_raw/mata_atlantica/fragment_forest_area.tif")

# Verificar resolucao, extensao e sistema de coordenadas
res(ma_fragmentos)
res(alt_classificado)
res(altitude)
crs(ma_fragmentos)
crs(alt_classificado)

# atribui um crs ao raster de altitude e ao de altitude classificado
crs(alt_classificado) <- "epsg:4326"
crs(altitude) <- "epsg:4326"

# reprojetar raster de fragmentos usando o raster de altitude classificado
ma_fragmentos_reproj <- project(ma_fragmentos, alt_classificado, method = "near")

## deixar raster de fragmentos na mesma extensao que o de altitude classificado
# recortar (Crop) o raster maior (ma_fragmentos)
ma_fragmentos_crop <- crop(ma_fragmentos_reproj, alt_classificado)
plot(ma_fragmentos_crop)

# Ajustar o Alinhamento (Resample)
# garante que os pixels do raster de fragmentos recortado se alinhem EXATAMENTE à grade de pixels do raster de altitude.
ma_fragmentos_final <- resample(ma_fragmentos_crop, 
                                alt_classificado, 
                                method = "near") 

plot(ma_fragmentos_final)
print(ma_fragmentos_final)

## filtrar as células que atendem aos dois critérios simultaneamente
# criterios: criar 60 pontos em cada categoria e que sejam em áreas florestadas

# Cria um raster onde só existem as categorias de altitude DENTRO da floresta.
altitude_floresta <- mask(alt_classificado, ma_fragmentos_final)
plot(altitude_floresta)
unique(altitude_floresta)

# Salva o raster filtrado no disco
writeRaster(altitude_floresta, "outputs/espacial/altitude_floresta_filtrado.tif", overwrite = TRUE)

## GERAR PONTOS DE AMOSTRAGEM
# Definir as variáveis de controle
n_pontos_por_categoria <- 60
categorias_presentes <- c(1, 2, 3) 
pontos_por_categoria <- list()

# Loop para fazer a amostragem em cada estrato separadamente
for (cat in categorias_presentes) {
  
  message(paste("- Processando Categoria:", cat))
  
  # 1. Cria uma máscara: células = cat -> 1, células != cat -> NA
  # Apenas as células da categoria 'cat' terão valor (1) e poderão ser amostradas.
  mascara_cat <- ifel(altitude_floresta == cat, 1, NA)
  
  # 2. Amostra N pontos DENTRO da categoria isolada
  # Aqui 'size' é um número único (60), evitando o erro relatado.
  pontos_cat <- spatSample(
    x = mascara_cat, 
    size = n_pontos_por_categoria, 
    method = "random", 
    na.rm = TRUE,      # Ignora as áreas NA (as outras categorias)
    xy = TRUE          # Retorna as coordenadas
  )
  
  # 3. Adiciona a categoria correta como um atributo
  if (!is.null(pontos_cat) && nrow(pontos_cat) > 0) {
    # Cria um SpatVector a partir do dataframe xy retornado
    pontos_vect <- vect(pontos_cat, geom = c("x", "y"), crs = crs(altitude_floresta))
    pontos_vect$categoria_alt <- cat
    pontos_por_categoria[[as.character(cat)]] <- pontos_vect
    
    message(paste("  -> Encontrados", nrow(pontos_cat), "pontos na Categoria", cat))
  } else {
    message(paste("  -> AVISO: Não foi possível amostrar pontos suficientes na Categoria", cat))
  }
}

# Acessa e plota o primeiro elemento da lista (Categoria 1)
plot(pontos_por_categoria$`1`, 
     col = "blue", 
     pch = 19, 
     cex = 0.5, 
     main = "Pontos de Amostragem: Categoria 1")

## salvar pontos em tabela
# categoria 1
pontos_categoria_1 <- pontos_por_categoria$`1`

# CONVERTER PARA DATA FRAME COM COORDENADAS
# as.data.frame() com geom=TRUE transforma o SpatVector em uma planilha,
tabela_categoria_1 <- as.data.frame(pontos_categoria_1, geom = "XY")

# Selecionar e Renomear as Colunas para facilitar a leitura
tabela_final_1 <- tabela_categoria_1[, c("x", "y", "categoria_alt")]
names(tabela_final_1) <- c("Longitude", "Latitude", "Categoria")

# categoria 2
pontos_categoria_2 <- pontos_por_categoria$`2`

# CONVERTER PARA DATA FRAME COM COORDENADAS
# as.data.frame() com geom=TRUE transforma o SpatVector em uma planilha,
tabela_categoria_2 <- as.data.frame(pontos_categoria_2, geom = "XY")

# Selecionar e Renomear as Colunas para facilitar a leitura
tabela_final_2 <- tabela_categoria_2[, c("x", "y", "categoria_alt")]
names(tabela_final_2) <- c("Longitude", "Latitude", "Categoria")

# categoria 3
pontos_categoria_3 <- pontos_por_categoria$`3`

# CONVERTER PARA DATA FRAME COM COORDENADAS
# as.data.frame() com geom=TRUE transforma o SpatVector em uma planilha,
tabela_categoria_3 <- as.data.frame(pontos_categoria_3, geom = "XY")

# Selecionar e Renomear as Colunas para facilitar a leitura
tabela_final_3 <- tabela_categoria_3[, c("x", "y", "categoria_alt")]
names(tabela_final_3) <- c("Longitude", "Latitude", "Categoria")

## Unir tabelas de pontos das categorias e salvar
tabela_combinada <- bind_rows(tabela_final_1, tabela_final_2, tabela_final_3)

# Verifica a contagem final
nrow(tabela_combinada)

# Salva o data frame combinado
write.csv(tabela_combinada_join, "outputs/tables/pontos.csv", row.names = FALSE)

## salvar pontos de cada categoria como spatvector
# definir pastas para salvar
pasta_saida_vetores <- "outputs/espacial/"

# Loop para iterar sobre a lista de pontos
for (nome_cat in names(pontos_por_categoria)) {
  
  # Acessa o SpatVector da categoria atual
  vetor_pontos <- pontos_por_categoria[[nome_cat]]
  
  # 1. Define o nome do arquivo de saída
  # Nomeia o arquivo com base no nome do elemento da lista (1, 2, ou 3)
  nome_arquivo <- paste0(pasta_saida_vetores, "pontos_categoria_", nome_cat, ".gpkg")
  
  # 2. Salva o SpatVector usando writeVector (formato GeoPackage)
  writeVector(
    vetor_pontos,
    filename = nome_arquivo,
    overwrite = TRUE
  )
  
  message(paste("- Categoria", nome_cat, "salva com", length(vetor_pontos), "pontos em:", nome_arquivo))
}
