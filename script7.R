# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 7 - Avaliação dos modelos, consenso e incerteza

#### Avaliando o desempenho dos modelos ####

# Calculando o ROC e TSS de cada modelo
evalprocnias1 = get_evaluations(procnias1model)
evalprocnias2 = get_evaluations(procnias2model)

# Sumário das avaliações do desempenho de cada modelo (rodada + set PA + algoritmo )
evalprocnias1

evalprocnias2

#Salvando os valores de desempenho (TSS, ROC, Sensibilidade, Especificidade) em uma tabela .csv
write.table(evalprocnias1,paste0("./Outputs/", "_", "evalprocnias1.csv"))

write.table(evalprocnias1,paste0("./Outputs/", "_", "evalprocnias2.csv"))

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
  prob.mean.weight.decay = "proportional" )

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
  prob.mean.weight.decay = "proportional" )

#### Avaliação de desempenho dos modelos de consenso, incluindo as métricas relacionadas a incerteza ####
evalprocnias1ensemble = get_evaluations(Ensemble1procnias)
evalprocnias2ensemble = get_evaluations(Ensemble2procnias)

#### Avaliação de desempenho dos modelos individuais (rodada + set PA + algoritmo), incluindo as métricas relacionadas a incerteza ####
evalprocnias1 = get_evaluations(procnias1model)
evalprocnias2 = get_evaluations(procnias2model)

# Sumário das avaliações do desempenho dos modelos individuais (rodada + set PA + algoritmo )
#Testing.data = valor de desempenho, podendo ser ROC, TSS ou Kappa
#Cutoff = Valor de corte para binarizar os modelos
#Sensitivity = % de presenças corretamente preditas
#Especificidade = % de ausências corretamente preditas

evalprocnias1

evalprocnias2

# Sumário das avaliações do desempenho dos modelos de consenso (rodada + set PA + algoritmo )
evalprocnias1ensemble

evalprocnias2ensemble

# Usando a sensibilidade (proporção de presenças corretamente preditas) e a especificidade (proporção de ausências corretamente preditas) como medidas de incerteza dos modelos pode-se observar diferenças entre os diferentes algoritmos usados para o consenso? Algum foi menos sensivel do que o outro, ou seja teve menor capacidade de prever as presenças corretamente?

#Salvando os valores de desempenho (TSS, ROC, Sensibilidade, Especificidade) em uma tabela .csv
write.table(evalprocnias1,paste0("./Outputs/", "_", "evalprocnias1.csv"))

write.table(evalprocnias2,paste0("./Outputs/", "_", "evalprocnias2.csv"))


write.table(evalprocnias1ensemble,paste0("./Outputs/", "_", "evalprocnias1ensemble.csv"))

write.table(evalprocnias2ensemble,paste0("./Outputs/", "_", "evalprocnias2ensemble.csv"))

#Plotando os valores de desempenho dos modelos gerados - modelos GLM, GAM, SRE
gg1_procnias1 =  models_scores_graph(procnias1model, metrics = c("TSS, ROC"), by = "models", plot = FALSE)
gg2_procnias1 =  models_scores_graph(procnias1model, metrics = c("ROC"), by = "data_set", plot = FALSE)
gg3_procnias1 =  models_scores_graph(procnias1model, metrics = c("ROC"), by = "cv_run", plot = FALSE)

grid.arrange(gg1_procnias1, gg2_procnias1, gg3_procnias1)

#Plotando os valores de desempenho dos modelos gerados - modelos RF, GBM, MaxEnt
gg1_procnias2 =  models_scores_graph(procnias2model, metrics = c("TSS, ROC"), by = "models", plot = FALSE)
gg2_procnias2 =  models_scores_graph(procnias2model, metrics = c("ROC"), by = "data_set", plot = FALSE)
gg3_procnias2 =  models_scores_graph(procnias2model, metrics = c("ROC"), by = "cv_run", plot = FALSE)

grid.arrange(gg1_procnias2, gg2_procnias2, gg3_procnias2)


#### Projetando no espaço geográfico as predições dos modelos de consenso ####
proj1ensemble=BIOMOD_EnsembleForecasting( projection.output = projec_procnias1, EM.output = Ensemble1procnias, binary.meth = "TSS", output.format = ".img")

proj2ensemble=BIOMOD_EnsembleForecasting( projection.output = projec_procnias2, EM.output = Ensemble2procnias, binary.meth = "TSS", output.format = ".img")

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

#### Projetando no espaço geográfico os modelos de consenso do presente nas condições climáticas futuras ####
proj1ensemble_futuro=BIOMOD_EnsembleForecasting( projection.output = projec_procnias1_futuro, EM.output = Ensemble1procnias, binary.meth = "TSS", output.format = ".img")

proj2ensemble_futuro=BIOMOD_EnsembleForecasting( projection.output = projec_procnias2_futuro, EM.output = Ensemble2procnias, binary.meth = "TSS", output.format = ".img")

#### Plotando as projeções dos modelos de consenso futuros (melhorar com os códigos da Mari)####
par(mfrow=c(2,2))

#Média 
plot(proj1ensemble_futuro, str.grep = "procnias_EMmeanByTSS_mergedAlgo_mergedRun_mergedData")

#Média ponderada
plot(proj1ensemble_futuro, str.grep = "procnias_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData")


#Mapas com métricas para avaliar as áreas com maiores ou menores incertezas (melhorar com os códigos da Mari)

#Coeficiente de variação
plot(proj1ensemble_futuro, str.grep = "procnias_EMcvByTSS_mergedAlgo_mergedRun_mergedData")

#Committee averaging
plot(proj1ensemble_futuro, str.grep = "procnias_EMcaByTSS_mergedAlgo_mergedRun_mergedData")

# Fim do script 7