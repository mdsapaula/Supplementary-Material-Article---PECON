### Material suplementar - Script R (Análise de correlação e Análise de Componentes Principais)###

#Autores
Erich de Freitas Mariano
Ana Paula de Medeiros
 
#Contato
mdsapaula@gmail.com


### Pré-Análise ###

# Limpar console e aumentar memória

rm(list = ls())

memory.limit(size = 1.75e13) 

# Instalar pacotes 

install.packages(c("raster", "rgdal", "corrplot", "RStoolbox", "vegan", "psych"), dep = T)

# Carregar pacotes 

library(raster) 
library(rgdal) 
library(corrplot) 
library(RStoolbox)
library(vegan) 
library(psych) 




### Análise de Componentes Principais - TODAS AS VARIÁVEIS ###
# Limpar console e aumentar memória

rm(list = ls())

memory.limit(size = 1.75e13) 

# Instalar pacotes 

install.packages(c("raster", "rgdal", "corrplot", "RStoolbox", "vegan", "psych"), dep = T)

# Carregar pacotes 

library(raster) 
library(rgdal) 
library(corrplot) 
library(RStoolbox)
library(vegan) 
library(psych) 
library(factoextra)

# Carregar diretório

setwd("D:/PIBIC")

# Criar uma pasta no diretório 

dir.create("PCA")

# Muda o diretório para a pasta criada 

setwd("D:/PIBIC/PCA/Todas_variaveis") 
 
# Ler dados extraídos dos rasters 

data <- read.table("variaveis_values_todas.csv", h=T,sep=",",dec=".")

dados_norm <- decostand(data, "normalize", MARGIN = 2, na.rm = T)

dados_norm

write.table(dados_norm, "variaveis_norm.csv", row.names = T, sep = ",")

data2 <- read.table("variaveis_norm.csv", h=T,sep=",",dec=".")

data2

# PCA
res.pca <- PCA(data)
pca <- prcomp(data2)
pca
eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = T)

var <- get_pca_var(res.pca)

fviz_pca_biplot(res.pca)
fviz_pca_var(pca,col.var = "contrib", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"), repel = TRUE, title="")
sd

summary(pca)

# Percentual de contribuição das componentes 

 std_list<-pca$sdev
 std_list_pct <- std_list / sum (std_list) * 100
 std_list_pct_sum=round(std_list_pct, 2)
 write.csv(std_list_pct_sum, file = "pca_percent_PC.csv"
screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores")

tiff("screeplot.tif", width = 18, height = 18, units = "cm", res = 300, compression = "lzw")

# Contribuição das variáveis 
d
pca$rotation[, 1:22]
round(pca$rotation[, 1:22], 2)
abs(round(pca$rotation[, 1:22], 2))

# Exportar tabela com a contribuição de cada variável 

write.table(abs(round(pca$rotation[, 1:22], 2)), "contr_vars_pca.xls", row.names = T, sep = "\t")

#Plotar PCA

biplot(pca)



#### Análise de correlação ####

# Carregar diretório

setwd("D:/PIBIC/Correlação")

# Criar uma pasta no diretório 

dir.create("Analise_correlação")

# Muda o diretório para a pasta criada 

setwd("Analise_correlação") 

setwd("D:/PIBIC/Correlação/Analise_correlação")

# Ler dados extraídos dos rasters

data <- read.table("variaveis_values.csv", h=T,sep=",",dec=".")

data 

# Correlação

cor <- cor(data)

cor

round(cor, 2)

abs(round(cor, 2))

ifelse(cor >= 0.7, "sim", "nao")

# Salvar matriz de correlação

write.table(round(cor, 2), "correlação_resultado.csv", row.names = T, sep = ",")

write.table(ifelse(cor >= 0.7, "sim", "nao"), "cor_resultado_afirmacao.csv", 
 row.names = T, sep = ",")

#Plota a correlação

corrplot(cor, type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1), title = "Correlacao entre variaveis Bioclimaticas")



### Análise de Componentes Principais - VARIÁVEIS NÃO CORRELACIONADAS###
# Limpar console e aumentar memória

rm(list = ls())

memory.limit(size = 1.75e13) 

# Instalar pacotes 

install.packages(c("raster", "rgdal", "corrplot", "RStoolbox", "vegan", "psych"), dep = T)

# Carregar pacotes 

library(raster) 
library(rgdal) 
library(corrplot) 
library(RStoolbox)
library(vegan) 
library(psych) 
library(factoextra)

# Carregar diretório

setwd("D:/PIBIC")

# Criar uma pasta no diretório 

dir.create("PCA")

# Muda o diretório para a pasta criada 

setwd("D:/PIBIC/PCA/Nao_correlacionadas") 
 
# Ler dados extraídos dos rasters 

data <- read.table("variaveis_values_nao correlacionadas.csv", h=T,sep=",",dec=".")

dados_norm <- decostand(data, "normalize", MARGIN = 2, na.rm = T)

dados_norm

write.table(dados_norm, "variaveis_norm.csv", row.names = T, sep = ",")

data2 <- read.table("variaveis_norm.csv", h=T,sep=",",dec=".")

data2

# PCA
res.pca <- PCA(data)
pca <- prcomp(data2)
pca
eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = T)

var <- get_pca_var(res.pca)

fviz_pca_biplot(res.pca)
fviz_pca_var(pca,col.var = "contrib", gradient.cols = c("#00afbb", "#e7b800", "#fc4e07"), repel = TRUE, title="")
sd

summary(pca)

# Percentual de contribuição das componentes 

 std_list<-pca$sdev
 std_list_pct <- std_list / sum (std_list) * 100
 std_list_pct_sum=round(std_list_pct, 2)
 write.csv(std_list_pct_sum, file = "pca_percent_PC.csv"
screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores")

tiff("screeplot.tif", width = 18, height = 18, units = "cm", res = 300, compression = "lzw")

# Contribuição das variáveis 
d
pca$rotation[, 1:9]
round(pca$rotation[, 1:9], 2)
abs(round(pca$rotation[, 1:9], 2))

# Exportar tabela com a contribuição de cada variável 

write.table(abs(round(pca$rotation[, 1:9], 2)), "contr_vars_pca.xls", row.names = T, sep = "\t")

#Plotar PCA

biplot(pca)

