# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 2 - Processamento e exploração visual de preditores ambientais

####################################
# Script 2 - Preditores ambientais #
####################################

#### Definindo diretório de trabalho ####
setwd("D:/Alunos/Joana Paula Oliveira/PPGBIO/Disciplinas/MDE - tutoria/Modelagem de Distribuição de Espécies-20210926T130618Z-001/Modelagem de Distribuição de Espécies") #Mude para o endereço da pasta da disciplina no seu computador
getwd()

#### Carregando pacotes ####
library(biomod2)
library(raster)
library(rgdal) #Carregar shapefiles
library(sdmpredictors) #Carregar as camadas online
#library(maptools)
library(usdm) #Teste de Inflação de Variância (VIF) para 'stacks'
library(ecospat)
library(CoordinateCleaner)
library(rgbif)
library(spocc)
library(spThin)
library(dplyr)
library(dismo)
library(gridExtra)

### Caso o corrplot não esteja instalado
#install.packages("corrplot")

#### Listando e carregando os datasets pre-existentes ####

list_datasets()

#Listando as camadas do WorldClim
list_layers("WorldClim")

#Obtendo camadas do Worldclim a partir da pasta da disciplina (se vc fez o download completo da pasta do GoogleDrive o caminho abaixo deve funcionar)
#Camadas topográficas
alt=raster("./Camadas/Presente/WC_alt_lonlat.tif")
alt
#Altitude

#Camadas climáticas
bio1=raster("./Camadas/Presente/WC_bio1_lonlat.tif")
#Temperatura Anual Média

bio3=raster("./Camadas/Presente/WC_bio3_lonlat.tif")
#Isotermalidade

bio4=raster("./Camadas/Presente/WC_bio4_lonlat.tif")
#Sazonalidade da temperatura

bio7=raster("./Camadas/Presente/WC_bio7_lonlat.tif")
#Variação Anual da Temperatura

bio12=raster("./Camadas/Presente/WC_bio12_lonlat.tif")
#Precipitação Anual

bio15=raster("./Camadas/Presente/WC_bio15_lonlat.tif")
#Sazonalidade da precipitação

#### OPCIONAL ####
#Obtendo camadas do WorldClim online em caso de não possuir/querer baixar

#alt=load_layers("WC_alt", datadir = "./Camadas")
#Altitude

#bio1=load_layers("WC_bio1", datadir = "./Camadas")
#Temperatura Média Anual

#bio3=load_layers("WC_bio3", datadir = "./Camadas")
#Isotermalidade

#bio4=load_layers("WC_bio4", datadir = "./Camadas")
#Sazonalidade da temperatura (SD x)

#bio7=load_layers("WC_bio7", datadir = "./Camadas")
#Variação Anual de Temperatura 

#bio12=load_layers("WC_bio12", datadir = "./Camadas")
#Precipitação Anual

#bio15=load_layers("WC_bio15", datadir = "./Camadas")
#Sazonalidade da precipitação (CV)

#### Explorando os detalhes das camadas climáticas ####

#Obtendo os limites e detalhes das camadas
bbox(alt); ncol(alt); nrow(alt) ; res(alt)
bbox(bio1); ncol(bio1); nrow(bio1) ; res(bio1)
bbox(bio3); ncol(bio3); nrow(bio3) ; res(bio3)
bbox(bio4); ncol(bio4); nrow(bio4) ; res(bio4)
bbox(bio7); ncol(bio7); nrow(bio7) ; res(bio7)
bbox(bio12); ncol(bio12); nrow(bio12) ; res(bio12)
bbox(bio15); ncol(bio15); nrow(bio15) ; res(bio15)
compareRaster(alt, bio1, bio3, bio4, bio7, bio12, bio15)

#### Visualizando as camadas climáticas ####
plot(alt, main="Altitude")
par(mfrow=c(3,2),mar=c(3,3,2,0)) #ajustar parâmetros gráficos para particionar a área do plot (mfrow) em 3 linhas e 2 colunas e ajustar as margens (mar)
plot(bio1, col=topo.colors(255), main="Temperatura Média Anual (bio1)")
plot(bio3, col=topo.colors(255), main="Isotermalidade (bio3)")
plot(bio4, col=topo.colors(255), main="Sazonalidade da temperatura (bio4)")
plot(bio7, col=topo.colors(255), main="Variação Anual de Temperatura (bio7)")
plot(bio12, col=topo.colors(255), main="Precipitação Anual (bio12)")
plot(bio15, col=topo.colors(255), main="Sazonalidade da precipitação (bio15)")
dev.off() #retorna os parâmetros gráficos para o default
###

#### Processando as camadas climáticas ####

#Cortando as camadas para o Brasil
#Carregando o mapa do Brasil
Brasil=getData('GADM', country= 'Brazil', level=1, path = "./Camadas")

#Cortando cada camada de uma vez para o Brasil e 'perfumando' minimamente os mapas

altBrasil=mask(crop(alt,Brasil),Brasil)
plot(altBrasil, main = "Altitude Brazil",xlab = "Longitude", ylab = "Latitude")

bio1Brasil=mask(crop(bio1,Brasil),Brasil)
plot(bio1Brasil, main = "bio1 Brasil",xlab = "Longitude", ylab = "Latitude")

bio3Brasil=mask(crop(bio3,Brasil),Brasil)
plot(bio3Brasil, main = "bio3 Brasil",xlab = "Longitude", ylab = "Latitude")

bio4Brasil=mask(crop(bio4,Brasil),Brasil)
plot(bio4Brasil, main = "bio4 Brazil",xlab = "Longitude", ylab = "Latitude")

bio7Brasil=mask(crop(bio7,Brasil),Brasil)
plot(bio7Brasil, main = "bio7 Brasil",xlab = "Longitude", ylab = "Latitude")

bio12Brasil=mask(crop(bio12,Brasil),Brasil)
plot(bio12Brasil, main = "bio12 Brasil",xlab = "Longitude", ylab = "Latitude")

bio15Brasil=mask(crop(bio15,Brasil),Brasil)
plot(bio15Brasil, main = "bio15 Brasil",xlab = "Longitude", ylab = "Latitude")


#Obtendo os limites e detalhes das camadas cortadas para o Brasil
bbox(altBrasil); ncol(altBrasil); nrow(altBrasil) ; res(altBrasil)
bbox(bio1Brasil); ncol(bio1Brasil); nrow(bio1Brasil) ; res(bio1Brasil)
bbox(bio3Brasil); ncol(bio3Brasil); nrow(bio3Brasil) ; res(bio3Brasil)
bbox(bio4Brasil); ncol(bio4Brasil); nrow(bio4Brasil) ; res(bio4Brasil)
bbox(bio7Brasil); ncol(bio7Brasil); nrow(bio7Brasil) ; res(bio7Brasil)
bbox(bio12Brasil); ncol(bio12Brasil); nrow(bio12Brasil) ; res(bio12Brasil)
bbox(bio15Brasil); ncol(bio15Brasil); nrow(bio15Brasil) ; res(bio15Brasil)
compareRaster(altBrasil, bio1Brasil, bio3Brasil, bio4Brasil, bio7Brasil, bio12Brasil, bio15Brasil)

#### Comparando as dimensões das camadas Brasil e Mundo
bbox(bio1); ncol(bio1); nrow(bio1) ; res(bio1)
bbox(bio1Brasil); ncol(bio1Brasil); nrow(bio1Brasil) ; res(bio1Brasil)

#### Unindo as variáveis em um unico 'stack' ####

biostack=stack(altBrasil,bio1Brasil,bio3Brasil,bio4Brasil,bio7Brasil,bio12Brasil,bio15Brasil)
plot(biostack)

#### Cortando as variáveis para o polígono da Araponga ####

procniaspolygon=readOGR("./Procnias/procnias_polygon.shp")

#Plotando o polígono da distribuição da Araponga no mapa do Brasil
plot(altBrasil,  main="Altitude Brasil")
plot(procniaspolygon, add=T)

#Cortando cada camada de uma vez para o poligono da Araponga e 'perfumando' minimamente os mapas

altprocnias=mask(crop(alt,procniaspolygon),procniaspolygon)
plot(altprocnias, main = "Altitude Procnias",xlab = "Longitude", ylab = "Latitude")
plot(procniaspolygon, add=T)

bio1procnias=mask(crop(bio1,procniaspolygon),procniaspolygon)
plot(bio1procnias, main = "bio1 Procnias",xlab = "Longitude", ylab = "Latitude")

bio3procnias=mask(crop(bio3,procniaspolygon),procniaspolygon)
plot(bio3procnias, main = "bio3 Procnias",xlab = "Longitude", ylab = "Latitude")

bio4procnias=mask(crop(bio4,procniaspolygon),procniaspolygon)
plot(bio4procnias, main = "bio4 Procnias",xlab = "Longitude", ylab = "Latitude")

bio7procnias=mask(crop(bio7,procniaspolygon),procniaspolygon)
plot(bio7procnias, main = "bio7 Procnias",xlab = "Longitude", ylab = "Latitude")

bio12procnias=mask(crop(bio12,procniaspolygon),procniaspolygon)
plot(bio12procnias, main = "bio12 Procnias",xlab = "Longitude", ylab = "Latitude")

bio15procnias=mask(crop(bio15,procniaspolygon),procniaspolygon)
plot(bio15procnias, main = "bio15 Procnias",xlab = "Longitude", ylab = "Latitude")

#Cortando o 'stack' com as variáveis para o polígono da Araponga
biostack1=mask(crop(biostack,procniaspolygon),procniaspolygon)
biostack1
summary(biostack1)

#Plotando o 'stack' com as camadas climáticas dentro do poligono da  Araponga
plot(biostack1)

#Checando existência de correlação e multicolinearidade entre as variáveis

#Correlação entre as camadas usando a função 'pairs'
pairs(biostack1)

#Visualizar a correlação entre as camadas usando o pacote corrplot
set.seed(1963) #seed number para sempre gerar os mesmos pontos abaixo
backgr <- randomPoints(biostack1, 10000)
absclim <- data.frame(extract(biostack1, backgr))
absclim.std <- data.frame(scale(absclim)) # Scale variables

library(corrplot)
M <- cor(absclim.std)
corrplot.mixed(M, upper = "ellipse", lower = "number",number.cex = 0.8,tl.cex = 0.8)

# Checagem de multicolinearidade entre as camadas - remoção stepwise com threshold = 0.7

vifcor(biostack1,th=0.7)
#th = valor de corte para correlação

vifstep(biostack1, th = 10)
#th = valor de corte para VIF


#Unindo as variáveis em um unico 'stack' após as análises de correlação e multicolinearidade

biostack2=stack(altprocnias,bio3procnias, bio7procnias,bio12procnias,bio15procnias)
biostack2

#Plotando o novo 'stack' com as camadas climáticas não correlacionadas e não-colinares dentro do poligono da  Araponga
plot(biostack2, main=c("Altitude", "Isotermalidade (bio3)", "Variação Anual de Temperatura (bio7)", "Precipitação Anual (bio12)","Sazonalidade da precipitação (bio15)"))

#Salvando o espaço de trabalho com todos os objetos num documento RData que pode ser carregado posteriormente.
save.image(file="script1.RData")

## Fim do script2
