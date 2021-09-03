# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 4 - Algoritmos de envelopes climáticos e generalizados

# Geração dos modelos usando os algoritmos de envelope (SRE) e generalizados (GLM e GAM)

#### Criando um diretorio 'Outputs' para guardar tabelas com resultados das modelagens ####
dir.create("Outputs")

#### Checando as opções 'default' de cada algoritmo ####
BIOMOD_ModelingOptions()

# Modificando as opções do Algoritmo GAM quanto ao numero de nós (k = 4) para evitar sobre ajuste 
modelo_op <- BIOMOD_ModelingOptions(GAM = list( algo = 'GAM_mgcv', type = 's_smoother', k = 4, interaction.level = 0, myFormula = NULL,  family = binomial(link = 'logit'), method = 'GCV.Cp', optimizer = c('outer','newton'), select = FALSE, knots = NULL,   paraPen = NULL, control = list(nthreads = 1, irls.reg = 0, epsilon = 1e-07, maxit = 200, trace = FALSE, mgcv.tol = 1e-07, mgcv.half = 15, rank.tol = 1.49011611938477e-08, nlm = list(ndigit=7, gradtol=1e-06, stepmax=2, steptol=1e-04, iterlim=200, check.analyticals=0), optim = list(factr=1e+07), newton = list(conv.tol=1e-06, maxNstep=5, maxSstep=2, maxHalf=30, use.svd=0), outerPIsteps = 0, idLinksBases = TRUE, scalePenalty = TRUE
, efs.lspmax = 15, efs.tol = 0.1, keepData = FALSE , scale.est = "fletcher", edge.correct = FALSE) ))

#### Definindo os parâmetros dos modelos, incluindo a escolha dos algoritmos, o numero de rodadas e a divisão do conjunto de dados entre treino e teste. ####
procnias1model = BIOMOD_Modeling(
  bm.procnias100disk, #objeto das pseudo-ausências
  models = c("GLM", "GAM","SRE"), #algoritmos de escolha
  models.options = modelo_op, #opções personalizadas ou padrões dos algoritmos
  NbRunEval = 3, #Numero de rodadas
  DataSplit = 70, #Divisão treino/teste
  Prevalence = 0.5,
  VarImport = 3, #permutações para gerar o valor de importância das variáveis
  models.eval.meth = c("TSS", "ROC"), #método de avaliação do desempenho dos modelos
  SaveObj = TRUE, #se os modelos serão salvos ou não
  rescal.all.models = F,
  do.full.models = FALSE,
  modeling.id = "NEWprocnias1model")

#Sumário do objeto com os modelos criados, onde é possível ver quais modelos foram gerados (set de pseudoausencia + rodada + algoritmo).
procnias1model

#### Obtendo a importância de cada variável usada nos modelos ####
var_import_procnias1=get_variables_importance(procnias1model) 

# obtendo os valores de importância de cada variável para cada modelo
var_import_procnias1

# obtendo os valores médios de importância de cada variável para cada algoritmo
var_import_procnias1=apply(procnias1model,c(1,2),mean)

# Salvando os valores de importancia das variáveis em um arquivo .csv
write.csv(var_import_procnias1,paste0("./Outputs/", "_", "var_import_procnias1.csv"))

#Qual variável foi a mais importante para cada algoritmo? Houveram diferenças entre algoritmos?

#### Criando curvas de resposta para cada algoritmo ####

#Carregando os modelos individuais que foram gerados acuma
procnias1_glm=BIOMOD_LoadModels(procnias1model, models = 'GLM')
procnias1_gam=BIOMOD_LoadModels(procnias1model, models = 'GAM')
procnias1_sre=BIOMOD_LoadModels(procnias1model, models = 'SRE')

#Plotando as curvas de resposta para cada modelo para cada variável, incluindo todas as rodadas.

glm_responsecurve= biomod2::response.plot2(models = procnias1_glm, Data = get_formal_data(procnias1model, 'expl.var'),show.variables = get_formal_data(procnias1model, 'expl.var.names'), do.bivariate = F, save.file = 'jpeg', ImageSize = 720, name= "GLM_curva_resposta", fixed.var.metric = 'median', legend = F, display_title = T, data_species = get_formal_data(procnias1model, 'resp.var') , plot = T)

gam_responsecurve= biomod2::response.plot2(models = procnias1_gam, Data = get_formal_data(procnias1model, 'expl.var'),show.variables = get_formal_data(procnias1model, 'expl.var.names'), do.bivariate = F, save.file = 'jpeg', ImageSize = 720, name= "GAM_curva_resposta", fixed.var.metric = 'median', legend = F, display_title = T, data_species = get_formal_data(procnias1model, 'resp.var') )

sre_responsecurve= biomod2::response.plot2(models = procnias1_sre, Data = get_formal_data(procnias1model, 'expl.var'),show.variables = get_formal_data(procnias1model, 'expl.var.names'), do.bivariate = F, save.file = 'jpeg', ImageSize = 720, name= "SRE_curva_resposta", fixed.var.metric = 'median', legend = F, display_title = T, data_species = get_formal_data(procnias1model, 'resp.var') )

# Fim do script 4