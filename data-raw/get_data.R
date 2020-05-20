library(httr)

# Pegar dados do INPE a partir do site deles
# Link: http://queimadas.dgi.inpe.br/queimadas/aq1km/

# URL para puxar dados da tabela "area queimada (km2)" 
url <- "http://queimadas.dgi.inpe.br/queimadas/aq1km/assets/data/Valores_Pixel_todos_biomas_urbano_2016_valor_1.csv?_=1589952820678"
g_inpe <- GET(url)

# Salvar dados em raw como csv
bin <- content(g_inpe, "raw")
writeBin(bin, "data-raw/data.csv")
