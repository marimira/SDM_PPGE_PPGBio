# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 6 - Projeção, transferabilidade e incerteza

#### Projetando as predições dos modelos no espaço geográfico ####

#Projetando todos os modelos e rodadas separadamente para GLM, GAM, SRE
projec_procnias1 <- BIOMOD_Projection(
  modeling.output = procnias1model, #objeto com os modelos criados
  new.env = biostack2, #variaveis ambientais onde os modelos vão ser projetados
  proj.name = "GLM_GAM_SRE", #nome da pasta com as projeções
  selected.models = "all",
  binary.meth = "TSS", #metodo para binarização
    output.format = ".img", compress = ' xz ' ,
  clamping.mask = T, do.stack = T
)

# Sumário do objeto com as projeções no espaço geográfico
projec_procnias1

#Projetando todos os modelos e rodadas separadamente para GBM, RF, MAXENT
projec_procnias2 <- BIOMOD_Projection(
  modeling.output = procnias2model, #objeto com os modelos criados
  new.env = biostack2, #variaveis ambientais onde os modelos vão ser projetados
  proj.name = "GBM_RF_MAX", #nome da pasta com as projeções
  selected.models = "all",
  binary.meth = "TSS", #metodo para binarização
  output.format = ".img", compress = ' xz ' ,
  clamping.mask = T, do.stack = T
)

# Sumário do objeto com as projeções no espaço geográfico
projec_procnias2

# Plotando as projeções por algoritmo (melhorar com os códigos da Mari para mapas)
plot(projec_procnias1, str.grep = "GLM")
plot(projec_procnias1, str.grep = "GAM")
plot(projec_procnias1, str.grep = "SRE")
plot(projec_procnias2, str.grep = "GBM")
plot(projec_procnias2, str.grep = "RF")
plot(projec_procnias2, str.grep = "MAXENT.Phillips")

#### Preparando as camadas para transferir as predições do modelo no tempo ####

#Camadas obtidas do WorldClim (CMIP 6) dentro dos Shared Socio-Economic Pathway 585 para os anos de 2060-2080 (https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained).

#Carregando as camadas futuras correspondentes as que foram usadas para modelar no tempo presente  (bio 3, bio 7, bio 12, bio 15)
#Nomes dos arquivos das camadas na pasta tem que ser os mesmos do presente para 'dar match' e ser possível projetar no futuro

bio3_2060=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Futuro/WC_bio3_lonlat.tif") 
#Isotermalidade

bio7_2060=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas//Futuro/WC_bio7_lonlat.tif")
#Variação Anual da Temperatura

bio12_2060=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Futuro/WC_bio12_lonlat.tif")
#Precipitação Anual

bio15_2060=raster("C:/Users/rhtar/OneDrive/R/ENM_PPGE/Camadas/Futuro/WC_bio15_lonlat.tif")
#Sazonalidade da precipitação

#### Cortando as camadas futuras para o polígono da Araponga ####
bio3procnias_futuro=mask(crop(bio3_2060,procniaspolygon),procniaspolygon)

#Formatando a bio3_2060 para ficar no mesmo 'range' de valores do que o bio 3 no tempo presente
bio3procnias_futuro=bio3procnias_futuro/100

bio7procnias_futuro=mask(crop(bio7_2060,procniaspolygon),procniaspolygon)

bio12procnias_futuro=mask(crop(bio12_2060,procniaspolygon),procniaspolygon)

bio15procnias_futuro=mask(crop(bio15_2060,procniaspolygon),procniaspolygon)

#### Unindo as variáveis futuras em um unico 'stack' ####
biostack_futuro=stack(altprocnias,bio3procnias_futuro,bio7procnias_futuro,bio12procnias_futuro,bio15procnias_futuro)

#### Plotando o 'stack' das variáveis futuras (melhorar com os códigos da Mari) ####
plot(biostack_futuro)

#### Projetando no espaço geográfico os modelos do presente nas condições climáticas futuras ####

projec_procnias1_futuro <- BIOMOD_Projection(
  modeling.output = procnias1model,
  new.env = biostack_futuro, #aqui você indica as camadas futuras que vão ser usadas para projetar
  proj.name = "Modelos Futuro",
  selected.models = "all",
  binary.meth = "TSS",
  output.format = ".img", compress = ' xz ' ,
  clamping.mask = T, do.stack = T
)


projec_procnias2_futuro <- BIOMOD_Projection(
  modeling.output = procnias2model,
  new.env = biostack_futuro, #aqui você indica as camadas futuras que vão ser usadas para projetar
  proj.name = "Modelos Futuro",
  selected.models = "all",
  binary.meth = "TSS",
  output.format = ".img", compress = ' xz ' ,
  clamping.mask = T, do.stack = T
)

#### Plotando os modelos individuais projetados no futuro (melhorar com os códigos da Mari) ####
plot(projec_procnias1_futuro, str.grep = "GLM")
plot(projec_procnias1_futuro, str.grep = "GAM")
plot(projec_procnias1_futuro, str.grep = "SRE")
plot(projec_procnias2_futuro, str.grep = "GBM")
plot(projec_procnias2_futuro, str.grep = "RF")
plot(projec_procnias2_futuro, str.grep = "MAXENT.Phillips")

#### Avaliando transferabilidade (códigos Mari) ####