# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 2 - Processamento e exploração visual de variáveis preditoras

##Inicio do script

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

#### Listando e carregando os datasets pree-existentes ####

list_datasets()

#Listando as camadas do WorldClim
list_layers("Freshwater")

#Obtendo camadas do Worldclim a partir da pasta da disciplina
#Camadas topográficas
alt=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Presente/WC_alt_lonlat.tif")
#Altitude

#Camadas climáticas
bio1=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Presente/WC_bio1_lonlat.tif")
#Temperatura Anual Média

bio3=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Presente/WC_bio3_lonlat.tif")
#Isotermalidade

bio4=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Presente/WC_bio4_lonlat.tif")
#Sazonalidade da temperatura

bio7=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/WC_bio7_lonlat.tif")
#Variação Anual da Temperatura

bio12=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Presente/WC_bio12_lonlat.tif")
#Precipitação Anual

bio15=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Presente/WC_bio15_lonlat.tif")
#Sazonalidade da precipitação

#### OPCIONAL ####
#Obtendo camadas do WorldClim online em caso de não possuir/querer baixar

#alt=load_layers("WC_alt", datadir = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")
#Altitude

#bio1=load_layers("WC_bio1", datadir = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")
#Temperatura Média Anual

#bio3=load_layers("WC_bio3", datadir = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")
#Isotermalidade

#bio4=load_layers("WC_bio4", datadir = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")
#Sazonalidade da temperatura (SD x)

#bio7=load_layers("WC_bio7", datadir = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")
#Variação Anual de Temperatura 

#bio12=load_layers("WC_bio12", datadir = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")
#Precipitação Anual

#bio15=load_layers("WC_bio15", datadir = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")
#Sazonalidade da precipitação (CV)

#### Visualizando as camadas climáticas ####
#Código Mari
###

#### Explorando os detalhes das camadas climáticas ####


#Obtendo os limites e detalhes das camadas
bbox(alt); ncol(alt); nrow(alt) ; res(alt)
bbox(bio1); ncol(bio1); nrow(bio1) ; res(bio1)
bbox(bio3); ncol(bio3); nrow(bio3) ; res(bio3)
bbox(bio4); ncol(bio4); nrow(bio4) ; res(bio4)
bbox(bio7); ncol(bio7); nrow(bio7) ; res(bio7)
bbox(bio12); ncol(bio12); nrow(bio12) ; res(bio12)
bbox(bio15); ncol(bio15); nrow(bio15) ; res(bio15)

#### Processando as camadas climáticas ####

#Cortando as camadas para o Brasil
#Carregando o mapa do Brasil
Brasil=getData('GADM', country= 'Brazil', level=1, path = "C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas")

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

#### Comparando as dimensões das camadas Brasil e Mundo
bbox(bio1); ncol(bio1); nrow(bio1) ; res(bio1)
bbox(bio1Brasil); ncol(bio1Brasil); nrow(bio1Brasil) ; res(bio1Brasil)

#### Unindo as variáveis em um unico 'stack' ####

biostack=stack(altBrasil,bio1Brasil,bio3Brasil,bio4Brasil,bio7Brasil,bio12Brasil,bio15Brasil)
plot(biostack)


#### Cortando as variáveis para o polígono da Araponga ####

procniaspolygon=readOGR("C:/Users/rhtar/OneDrive/R/ENM_PPGE/procnias_polygon.shp")

#Plotando o poligono da distribuição da Araponga no mapa do Brasil
plot(procniaspolygon)  
#refinar com os codigos da Mari

#Cortando cada camada de uma vez para o poligono da Araponga e 'perfumando' minimamente os mapas

altprocnias=mask(crop(alt,procniaspolygon),procniaspolygon)
plot(altprocnias, main = "Altitude Procnias",xlab = "Longitude", ylab = "Latitude")

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

#Cortando o 'stack' com as variaveis para o poligono da Araponga
biostack1=mask(crop(biostack,procniaspolygon),procniaspolygon)
biostack1
summary(biostack1)

#Plotando o 'stack' com as camadas climáticas dentro do poligono da  Araponga
plot(biostack1)

#Checando existência de correlação e multicolinearidade entre variáveis

#Correlação entre as camadas usando a função 'pairs'
pairs(biostack1)


# Checagem de multicolinearidade entre as camadas - remoção stepwise com threshold = 0.7

vifcor(biostack1,th=0.7)
#th = valor de corte para correlação

vifstep(biostack1, th = 10)
#th = valor de corte para VIF


#Unindo as variáveis em um unico 'stack' após as análises de correlação e multicolinearidade

biostack2=stack(altprocnias,bio3procnias, bio7procnias,bio12procnias,bio15procnias)
biostack2

#Plotando o novo 'stack' com as camadas climáticas não correlacionadas e não-colinares dentro do poligono da  Araponga
plot(biostack2)

## Fim do script
