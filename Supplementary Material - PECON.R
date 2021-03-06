### Material suplementar - Script R (An�lise de correla��o e An�lise de Componentes Principais)###

#Autores
Erich de Freitas Mariano
Ana Paula de Medeiros
 
#Contato
mdsapaula@gmail.com


### Pr�-An�lise ###

# Limpar console e aumentar mem�ria

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




### An�lise de Componentes Principais - TODAS AS VARI�VEIS ###
# Limpar console e aumentar mem�ria

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

# Carregar diret�rio

setwd("D:/PIBIC")

# Criar uma pasta no diret�rio 

dir.create("PCA")

# Muda o diret�rio para a pasta criada 

setwd("D:/PIBIC/PCA/Todas_variaveis") 
 
# Ler dados extra�dos dos rasters 

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

# Percentual de contribui��o das componentes 

 std_list<-pca$sdev
 std_list_pct <- std_list / sum (std_list) * 100
 std_list_pct_sum=round(std_list_pct, 2)
 write.csv(std_list_pct_sum, file = "pca_percent_PC.csv"
screeplot(pca, main = "Contribui��o de cada PC", ylab = "Autovalores")

tiff("screeplot.tif", width = 18, height = 18, units = "cm", res = 300, compression = "lzw")

# Contribui��o das vari�veis 
d
pca$rotation[, 1:22]
round(pca$rotation[, 1:22], 2)
abs(round(pca$rotation[, 1:22], 2))

# Exportar tabela com a contribui��o de cada vari�vel 

write.table(abs(round(pca$rotation[, 1:22], 2)), "contr_vars_pca.xls", row.names = T, sep = "\t")

#Plotar PCA

biplot(pca)



#### An�lise de correla��o ####

# Carregar diret�rio

setwd("D:/PIBIC/Correla��o")

# Criar uma pasta no diret�rio 

dir.create("Analise_correla��o")

# Muda o diret�rio para a pasta criada 

setwd("Analise_correla��o") 

setwd("D:/PIBIC/Correla��o/Analise_correla��o")

# Ler dados extra�dos dos rasters

data <- read.table("variaveis_values.csv", h=T,sep=",",dec=".")

data 

# Correla��o

cor <- cor(data)

cor

round(cor, 2)

abs(round(cor, 2))

ifelse(cor >= 0.7, "sim", "nao")

# Salvar matriz de correla��o

write.table(round(cor, 2), "correla��o_resultado.csv", row.names = T, sep = ",")

write.table(ifelse(cor >= 0.7, "sim", "nao"), "cor_resultado_afirmacao.csv", 
 row.names = T, sep = ",")

#Plota a correla��o

corrplot(cor, type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1), title = "Correlacao entre variaveis Bioclimaticas")



### An�lise de Componentes Principais - VARI�VEIS N�O CORRELACIONADAS###
# Limpar console e aumentar mem�ria

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

# Carregar diret�rio

setwd("D:/PIBIC")

# Criar uma pasta no diret�rio 

dir.create("PCA")

# Muda o diret�rio para a pasta criada 

setwd("D:/PIBIC/PCA/Nao_correlacionadas") 
 
# Ler dados extra�dos dos rasters 

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

# Percentual de contribui��o das componentes 

 std_list<-pca$sdev
 std_list_pct <- std_list / sum (std_list) * 100
 std_list_pct_sum=round(std_list_pct, 2)
 write.csv(std_list_pct_sum, file = "pca_percent_PC.csv"
screeplot(pca, main = "Contribui��o de cada PC", ylab = "Autovalores")

tiff("screeplot.tif", width = 18, height = 18, units = "cm", res = 300, compression = "lzw")

# Contribui��o das vari�veis 
d
pca$rotation[, 1:9]
round(pca$rotation[, 1:9], 2)
abs(round(pca$rotation[, 1:9], 2))

# Exportar tabela com a contribui��o de cada vari�vel 

write.table(abs(round(pca$rotation[, 1:9], 2)), "contr_vars_pca.xls", row.names = T, sep = "\t")

#Plotar PCA

biplot(pca)

