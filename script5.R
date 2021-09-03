# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 5 - Algoritmos de classificação e aprendizado de máquina

# Geração dos modelos usando os algoritmos de classificação (GBM, RF) e aprendizado de máquina (MaxEnt)

#### Checando as opções 'default' de cada algoritmo ####
BIOMOD_ModelingOptions()


#### Formatando os dados, camadas e fazendo conexão para o formato do Maxent ####
# Para trabalhar com o MAXENT nós temos que criar um caminho para um diretório contendo nossas variaveis explanatórias em ascii

maxent.background.dat.dir <- "maxent_bg"
dir.create(maxent.background.dat.dir, showWarnings = FALSE, recursive = TRUE)

## salvando as variáveis explanatórias em .ascii
for(var_ in names(biostack2)){
  cat("\n> saving", paste0(var_, ".asc"))
  writeRaster(subset(biostack2, var_), 
              filename = file.path(maxent.background.dat.dir, paste0(var_, ".asc")),
              overwrite = TRUE)
}
## definindo o camimnho para o arquivo maxent.jar
path.to.maxent.jar <- file.path(getwd(), "maxent.jar")

#### Definindo os parâmetros dos modelos, incluindo a escolha dos algoritmos, o numero de rodadas e a divisão do conjunto de dados entre treino e teste. ####
procnias2model = BIOMOD_Modeling(
  bm.procnias100disk, #objeto das pseudo-ausências
  models = c("GBM", "RF","MAXENT.Phillips"), #algoritmos de escolha
  models.options = modelo_op2, #opções personalizadas ou padrões dos algoritmos
  NbRunEval = 3, #Numero de rodadas
  DataSplit = 70, #Divisão treino/teste
  Prevalence = 0.5,
  VarImport = 3, #permutações para gerar o valor de importância das variáveis
  models.eval.meth = c("TSS", "ROC"), #método de avaliação do desempenho dos modelos
  SaveObj = TRUE, #se os modelos serão salvos ou não
  rescal.all.models = F,
  do.full.models = FALSE,
  modeling.id = "NEWprocnias2model")

#Sumário do objeto com os modelos criados, onde é possível ver quais modelos foram gerados (set de pseudoausencia + rodada + algoritmo).
procnias2model

#### Obtendo a importância de cada variável usada nos modelos ####
var_import_procnias2=get_variables_importance(procnias2model) 

# obtendo os valores de importância de cada variável para cada modelo
var_import_procnias2

# obtendo os valores médios de importância de cada variável para cada algoritmo
var_import_procnias2=apply(procnias2model,c(1,2),mean)

# Salvando os valores de importancia das variáveis em um arquivo .csv
write.csv(var_import_procnias2, file="var_import_procnias2.csv")

#Qual variável foi a mais importante para cada algoritmo? Houveram diferenças entre algoritmos?

#### Criando curvas de resposta para cada algoritmo ####

#Carregando os modelos individuais que foram gerados acuma
procnias2_gbm=BIOMOD_LoadModels(procnias2model, models = 'GBM')
procnias2_rf=BIOMOD_LoadModels(procnias2model, models = 'RF')
procnias2_MaxEnt=BIOMOD_LoadModels(procnias2model, models = 'MAXENT.Phillips')

#Plotando as curvas de resposta para cada modelo para cada variável, incluindo todas as rodadas.

gbm_responsecurve= biomod2::response.plot2(models = procnias2_gbm, Data = get_formal_data(procnias2model, 'expl.var'),show.variables = get_formal_data(procnias2model, 'expl.var.names'), do.bivariate = F, save.file = 'jpeg', ImageSize = 720, name= "gbm_curva_resposta", fixed.var.metric = 'median', legend = F, display_title = T, data_species = get_formal_data(procnias2model, 'resp.var') , plot = T)

rf_responsecurve= biomod2::response.plot2(models = procnias2_rf, Data = get_formal_data(procnias2model, 'expl.var'),show.variables = get_formal_data(procnias2model, 'expl.var.names'), do.bivariate = F, save.file = 'jpeg', ImageSize = 720, name= "rf_curva_resposta", fixed.var.metric = 'median', legend = F, display_title = T, data_species = get_formal_data(procnias2model, 'resp.var') )

MaxEnt_responsecurve= biomod2::response.plot2(models = procnias2_MaxEnt, Data = get_formal_data(procnias2model, 'expl.var'),show.variables = get_formal_data(procnias2model, 'expl.var.names'), do.bivariate = F, save.file = 'jpeg', ImageSize = 720, name= "MaxEnt_curva_resposta", fixed.var.metric = 'median', legend = F, display_title = T, data_species = get_formal_data(procnias2model, 'resp.var'))

# Fim do Script 5
