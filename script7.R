# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 7 - Avaliação dos modelos, consenso e incerteza

######################################################
# Script 7 - Avaliação dos modelos, consenso e incerteza #
######################################################

# Geração dos modelos usando os algoritmos de classificação (GBM, RF) e aprendizado de máquina (MaxEnt)

#### Definindo area de trabalho ####
setwd("D:\OneDrive\Documentos\R\Scripts\21_MDE_PPGE") #Mude para o endereco da pasta da disciplina no seu computador
getwd()

#### Carregando pacotes ####
library(biomod2)
library(gridExtra)
library(raster)

#### Carregando os objetos do script 2, 3 e 4 ####
load("script2.RData")
load("script3.RData")
load("script4.RData")
load("script5.RData")
load("Script6.RData")

#### Avaliando o desempenho dos modelos ####

# Calculando o ROC e TSS de cada modelo
evalprocnias1 = get_evaluations(procnias1model)
evalprocnias2 = get_evaluations(procnias2model)

# Sumário das avaliações do desempenho de cada modelo (rodada + set PA + algoritmo )
evalprocnias1
evalprocnias2

#Salvando os valores de desempenho (TSS, ROC, Sensibilidade, Especificidade) em uma tabela .csv
write.table(evalprocnias1,paste0("./Outputs/", "evalprocnias1.csv"))
write.table(evalprocnias2,paste0("./Outputs/", "evalprocnias2.csv"))

#Plotando os valores de desempenho dos modelos gerados
#Modelos GLM, GAM, SRE
gg1_procnias1 =  models_scores_graph(procnias1model, metrics = c("TSS", "ROC"), by = "models", plot = FALSE)
gg2_procnias1 =  models_scores_graph(procnias1model, metrics = c("TSS", "ROC"), by = "data_set", plot = FALSE)
gg3_procnias1 =  models_scores_graph(procnias1model, metrics = c("TSS", "ROC"), by = "cv_run", plot = FALSE)

grid.arrange(gg1_procnias1, gg2_procnias1, gg3_procnias1)

#Modelos GBM, RF, MAXENT
gg1_procnias2 =  models_scores_graph(procnias2model, metrics = c("TSS", "ROC"), by = "models", plot = FALSE)
gg2_procnias2 =  models_scores_graph(procnias2model, metrics = c("TSS", "ROC"), by = "data_set", plot = FALSE)
gg3_procnias2 =  models_scores_graph(procnias2model, metrics = c("TSS", "ROC"), by = "cv_run", plot = FALSE)

grid.arrange(gg1_procnias2, gg2_procnias2, gg3_procnias2)

#### Construindo o modelo de consenso ####
# Existem várias maneiras de parameterizar o modelo de consenso, incluindo a métrica de desempenho, o valor de corte e os tipos de modelos consenso que queremos obter

#Modelo de consenso para os modelos GLM, GAM, SRE que tiveram um desempenho maior do que TSS > 0.7
Ensemble1procnias <- BIOMOD_EnsembleModeling(
  modeling.output = procnias1model, # modelos individuais
  chosen.models = "all" ,
  em.by= "all" , #tipo de combinação para produzir o modelo de consenso (PA + Rodada + Algoritmo)
  eval.metric = c("TSS"), #metrica de desempenho usada para fazer o consenso
  eval.metric.quality.threshold = c(0.7), #valor de corte para os modelos usados para o consenso
  prob.mean = T, #consenso com a média das predições entre os modelos individuais
  prob.cv = T, #consenso com o coeficiente de variação das predições entre os modelos individuais
  prob.ci = F, #consenso com o intervalo de confiança das predições entre os modelos individuais
  prob.ci.alpha = 0.05,
  prob.median = F, #consenso com a mediana das predições entre os modelos individuais
  committee.averaging = T, #consenso com o comitê de acordo/desacordo das predições entre os modelos individuais
  prob.mean.weight = T, #consenso com a média ponderada das predições entre os modelos individuais
  prob.mean.weight.decay = "proportional" ) #maneira de calcular a ponderação das médias

#Modelo de consenso para os modelos GBM, RF, MAXENT que tiveram um desempenho maior do que TSS > 0.7
Ensemble2procnias <- BIOMOD_EnsembleModeling(
  modeling.output = procnias2model, # modelos individuais
  chosen.models = "all" ,
  em.by= "all" , #tipo de união para consenso (PA + Rodada + Algoritmo)
  eval.metric = c("TSS"), #metrica de desempenho usada para fazer o consenso
  eval.metric.quality.threshold = c(0.7), #valor de corte para os modelos usados para o consenso
  prob.mean = T, #consenso com a média das predições entre os modelos individuais
  prob.cv = T, #consenso com o coeficiente de variação das predições entre os modelos individuais
  prob.ci = F, #consenso com o intervalo de confiança das predições entre os modelos individuais
  prob.ci.alpha = 0.05,
  prob.median = F, #consenso com a mediana das predições entre os modelos individuais
  committee.averaging = T, #consenso com o comitê de acordo/desacordo das predições entre os modelos individuais
  prob.mean.weight = T, #consenso com a média ponderada das predições entre os modelos individuais
  prob.mean.weight.decay = "proportional") #maneira de calcular a ponderação das médias

#### Avaliação de desempenho dos modelos de consenso, incluindo as métricas relacionadas a incerteza ####
evalprocnias1ensemble = get_evaluations(Ensemble1procnias)
evalprocnias2ensemble = get_evaluations(Ensemble2procnias)

# Sumário das avaliações do desempenho dos modelos individuais (rodada + set PA + algoritmo )
#Testing.data = valor de desempenho, podendo ser ROC, TSS ou Kappa
#Cutoff = Valor de corte para binarizar os modelos
#Sensitivity = % de presenças corretamente preditas
#Especificidade = % de ausências corretamente preditas

# Sumário das avaliações do desempenho dos modelos de consenso (rodada + set PA + algoritmo )
evalprocnias1ensemble
evalprocnias2ensemble

# Usando a sensibilidade (proporção de presenças corretamente preditas) e a especificidade (proporção de ausências corretamente preditas) como medidas de incerteza dos modelos pode-se observar diferenças entre os diferentes algoritmos usados para o consenso? Algum foi menos sensivel do que o outro, ou seja teve menor capacidade de prever as presenças corretamente?

#Salvando os valores de desempenho (TSS, ROC, Sensibilidade, Especificidade) em uma tabela .csv

write.table(evalprocnias1ensemble,paste0("./Outputs/", "evalprocnias1ensemble.csv"))

write.table(evalprocnias2ensemble,paste0("./Outputs/", "evalprocnias2ensemble.csv"))



#### Projetando no espaço geográfico as predições dos modelos de consenso ####
proj1ensemble=BIOMOD_EnsembleForecasting( projection.output = projec_procnias1, #objeto com as predições dos modelos individuais
                                          EM.output = Ensemble1procnias, #objeto com a modelagem consenso
                                          binary.meth = "TSS", #método de avaliação de desempenho para transformar as predições contínuas de adequabilidade em predições binárias (presença/ausência) 
                                          output.format = ".img" #formato par salvar os mapas
                                          )

proj2ensemble=BIOMOD_EnsembleForecasting( projection.output = projec_procnias2, #objeto com as predições dos modelos individuais
                                          EM.output = Ensemble2procnias, #objeto com a modelagem consenso
                                          binary.meth = "TSS", #método de avaliação de desempenho para transformar as predições contínuas de adequabilidade em predições binárias (presença/ausência) 
                                          output.format = ".img" #formato par salvar os mapas
)


#### Plotando as projeções dos modelos de consenso (melhorar com os códigos da Mari)####

#Média 
plot(proj1ensemble, str.grep = "procnias_EMmeanByTSS_mergedAlgo_mergedRun_mergedData")

#Média ponderada
plot(proj1ensemble, str.grep = "procnias_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData")

#Mapas com métricas para avaliar as áreas com maiores ou menores incertezas (melhorar com os códigos da Mari)

#Coeficiente de variação
plot(proj1ensemble, str.grep = "procnias_EMcvByTSS_mergedAlgo_mergedRun_mergedData")

#Committee averaging
plot(proj1ensemble, str.grep = "procnias_EMcaByTSS_mergedAlgo_mergedRun_mergedData")

#Mapa binarizado

#Primeiro temos que carregar o arquivo que foi salvo no processo de modelagem e projeção - GLM_GAM_SRE
procnias1ensemble_bin=raster("./Procnias/proj_GLM_GAM_SRE/proj_GLM_GAM_SRE_procnias_ensemble_TSSbin.img")

#Agora o plot
plot(procnias1ensemble_bin)

#Carregando o mapa binarizado dos modelos com os algoritmos RF, GBM, MAXENT
procnias2ensemble_bin=raster("./Procnias/proj_GBM_RF_MAX/proj_GBM_RF_MAX_procnias_ensemble_TSSbin.img")

#Agora o plot
plot(procnias2ensemble_bin)

#### Projetando no espaço geográfico os modelos de consenso do presente nas condições climáticas futuras ####
proj1ensemble_futuro=BIOMOD_EnsembleForecasting( projection.output = projec_procnias1_futuro, #output gerado da projeção dos modelos individuais no futuro
                                                 EM.output = Ensemble1procnias, #output com o consenso de modelos gerados para o persente
                                                 binary.meth = "TSS", output.format = ".img")

proj2ensemble_futuro=BIOMOD_EnsembleForecasting( projection.output = projec_procnias2_futuro, #output gerado da projeção dos modelos individuais no futuro
                                                 EM.output = Ensemble2procnias, #output com o consenso de modelos gerados para o persente
                                                 binary.meth = "TSS", output.format = ".img")

#### Plotando as projeções dos modelos de consenso futuros (melhorar com os códigos da Mari)####

#Média 
plot(proj1ensemble_futuro, str.grep = "procnias_EMmeanByTSS_mergedAlgo_mergedRun_mergedData")

#Média ponderada
plot(proj1ensemble_futuro, str.grep = "procnias_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData")

#Média 
plot(proj2ensemble_futuro, str.grep = "procnias_EMmeanByTSS_mergedAlgo_mergedRun_mergedData")

#Média ponderada
plot(proj2ensemble_futuro, str.grep = "procnias_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData")

#Mapas com métricas para avaliar as áreas com maiores ou menores incertezas (melhorar com os códigos da Mari)

#Coeficiente de variação
plot(proj1ensemble_futuro, str.grep = "procnias_EMcvByTSS_mergedAlgo_mergedRun_mergedData")

#Committee averaging
plot(proj1ensemble_futuro, str.grep = "procnias_EMcaByTSS_mergedAlgo_mergedRun_mergedData")

#Mapa binarizado
#Primeiro temos que carregar o arquivo que foi salvo no processo de modelagem e projeção
#Modelos GLM_GAM_SRE
procnias1ensemble_binfuturo=raster("./Procnias/proj_Modelos Futuro_GLM_GAM_SRE/proj_Modelos Futuro_GLM_GAM_SRE_procnias_ensemble_TSSbin.img")

#Agora o plot
plot(procnias1ensemble_binfuturo)

#Modelos RF_GBM_MAXENT
procnias2ensemble_binfuturo=raster("./Procnias/proj_Modelos Futuro_RF_BRT_MAXENT/proj_Modelos Futuro_RF_BRT_MAXENT_procnias_ensemble_TSSbin.img")

#Agora o plot
plot(procnias2ensemble_binfuturo)

#Salvando o espaço de trabalho com todos os objetos num documento RData que pode ser carregado posteriormente.
save.image(file="script7.RData")

# Fim do script 7