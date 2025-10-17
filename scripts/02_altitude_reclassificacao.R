### baixando raster de altitude da Bahia e classificando o raster ###

# carregar pacotes
library(terra)
library(utils)

## Carregar dados de altitude para a Bahia e unir quadriculas
# Define a pasta de destino para salvar o mosaico
pasta_destino <- "outputs/espacial/"

# Definir a lista de caminhos dos arquivos raster de quadriculas
caminhos_rasters <- c(
  "data/data_raw/MDE_TOPODATA/13S405ZN.tif", 
  "data/data_raw/MDE_TOPODATA/14S405ZN.tif",
  "data/data_raw/MDE_TOPODATA/15S405ZN.tif",
  "data/data_raw/MDE_TOPODATA/16S405ZN.tif",
  "data/data_raw/MDE_TOPODATA/17S405ZN.tif",
  "data/data_raw/MDE_TOPODATA/18S405ZN.tif"
)

# unir as quadriculas e criar um unico raster
if (length(caminhos_rasters) < 2) {
  message("Atenção: Apenas um arquivo raster ou nenhum foi definido. Mosaico não realizado.")
} else {
  
  # CRIAR O OBJETO COM TODAS AS QUADRÍCULAS
  message(paste("\nCarregando", length(caminhos_rasters), "quadrículas para o objeto..."))
  
  # Carrega cada arquivo em um objeto SpatRaster e guarda numa lista
  rasters_para_unir <- lapply(caminhos_rasters, function(c) {
    tryCatch({
      rast(c)
    }, error = function(e) {
      message(paste("AVISO: Falha ao carregar o arquivo:", c))
      return(NULL)
    })
  })
  
  # Remove entradas que falharam ao carregar (se houver)
  rasters_para_unir <- Filter(Negate(is.null), rasters_para_unir)
    
    # UNIR AS QUADRÍCULAS USANDO O OBJETO
    mosaico_final <- tryCatch({
      # Usar 'do.call' e 'mosaic' para unir os rasters da lista
      # Usar fun="mean" se houver sobreposição e você precisar de um valor médio
      do.call(mosaic, c(rasters_para_unir, fun = "mean")) 
    }, error = function(e) {
      message("!!! ERRO ao realizar o Mosaico. Verifique a compatibilidade (CRS, resolução).")
      print(e)
      return(NULL)
    })
    
    if (!is.null(mosaico_final)) {
      # SALVAR O MOSAICO FINAL
      caminho_saida_mosaico <- paste0(pasta_destino, "altitude_mosaico.tif")
      
      writeRaster(
        mosaico_final, 
        filename = caminho_saida_mosaico, 
        datatype = "INT2U", 
        overwrite = TRUE
      )
      
      message("\n*** Processo concluído! Mosaico salvo em:")
      print(caminho_saida_mosaico)
      plot(mosaico_final, main = "Modelo Digital de Elevação Mosaico (TOPODATA)")
    }
  }


## classificar do raster em 3 categorias
# Definir a matriz de reclassificacao
reclass_matrix <- matrix(
  c(-Inf, 200, 1,      # 1: Terras Baixas (0-200m)
    200, 600, 2,  # 2: Floresta Submontana (201-600m)
    600, Inf, 3   # 3: Floresta Montana (> 600m)
  ), ncol = 3, byrow = TRUE
)

# Aplicar a classificação (Cria a nova variável)
mosaico_classificado <- classify(
  mosaico_final, 
  reclass_matrix, 
  include.lowest = TRUE,
  right = FALSE 
)
unique(mosaico_classificado)
plot(mosaico_classificado)
# Salvar o raster classificado
writeRaster(mosaico_classificado, "outputs/espacial/altitude_classificado.tif", datatype = "INT1U", # Tipo de dado pequeno, pois só tem 1, 2 e 3
  overwrite = TRUE
  )