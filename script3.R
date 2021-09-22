# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 3 - Registros de ocorrência, pseudo-ausências e background.


#### Carregando os registros de ocorrência ####

#### Obtendo os registros do diretório ####
procnias=read.csv("./Procnias/procnias_nudicollis.csv",header=T,  sep = ";")
View(procnias)

#### Checando inconsistências relacionadas aos registros de ocorrências - problemas, filtro e limpeza ####
# Caso existam linhas nas colunas das coordenadas sem informações (NA), a função as remove
procnias_na <- procnias %>% tidyr::drop_na(decimalLongitude, decimalLatitude)
View(procnias_na)
#Quantos registros foram removidos por não conter coordenadas?

# Limpando registros com outros problemas referente as coordenadas 
# Marcando os registros potencialmente problemáticos 
flags_spatial <- CoordinateCleaner::clean_coordinates(
  x = procnias_na, 
  species = "species",
  lon = "decimalLongitude", 
  lat = "decimalLatitude",
  tests = c("capitals", # raio ao redor de capitais
            "centroids", # raio ao redor de centroides de paises e provincias
            "duplicates", # duplicatas
            "equal", # coordenadas iguais
            "gbif", # raio ao redor da sede da GBIF
            "institutions", # raio ao redor de instituicoes de pesquisa em biodiversidade
            "seas", # pontos no mar
            "urban", # pontos dentro de areas urbanas
            "validity", # ponto de fora do sistema de coordenadas
            "zeros" # zeros e pontos onde lat = lon 
  )
)

# Resultado dos registros que foram problematicos
# TRUE = coordenadas 'limpas'
# FALSE = coordenadas potencialmente problematicas
head(flags_spatial)
summary(flags_spatial)

# Excluindo os pontos considerados como problematicos na analise anterior
procnias_f <- procnias_na %>% 
  dplyr::filter(flags_spatial$.summary == TRUE)
#Quantos registros ficaram retidos após a remoção dos problematicos?

#### Limpando e selecionando as colunas de interesse ####
procnias_f = procnias_f %>%
  dplyr::select(species, decimalLongitude, decimalLatitude)

#Checando a extensão do arquivo limpo
nrow(procnias_f)
colnames(procnias_f)

#### Plotando os registros de ocorrência filtrados para visualização ####

plot(alt, xlim=c(-80,-30),ylim=c(-40,10), main="Presence points by altitude")
data(wrld_simpl)
plot(wrld_simpl, add=TRUE, border='darkgray', lwd=1)
points(procnias_f$decimalLongitude, procnias_f$decimalLatitude, col='blue', cex=0.3)

#### Filtrando os registros de ocorrência baseado em uma distância 'x' para evitar potenciais problemas de autocorrelação espacial ####
procnias_thin <- 
  thin( loc.data = procnias_f, 
        lat.col = "decimalLatitude", long.col = "decimalLongitude", 
        spec.col = "species", 
        thin.par = 50 #distancia em km que os registros vão ficar separados#  
        ,reps = 5, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 2, 
        out.dir = "./Procnias", out.base = "procnias_thinned", 
        write.log.file = TRUE,
        log.file = "procnias_thinned_log.txt")

#Explorando visualmente o efeito do processo de filtragem espacial dos dados
plotThin(procnias_thin)

#Valores chegaram no valor máximo com as repetições usadas. Dê uma olhada no arquivo 'log' com todos os detalhes

#Carregando o novo arquivo com o processo de filtragem espacial realizado.
procnias_f=read.csv("./Procnias/procnias_thinned_thin1.csv", sep=",")
View(procnias_f)

#### Plotando os registros de ocorrência filtrados espacialmente para visualização ####

plot(alt, xlim=c(-80,-30),ylim=c(-40,10), main="Presence points by altitude")
plot(wrld_simpl, add=TRUE, border='darkgray', lwd=1)
points(procnias_f$decimalLongitude, procnias_f$decimalLatitude, col='blue', cex=0.3)


#Inserindo uma coluna contendo todos os registros igual a 1
resp.occ=as.numeric("resp.occ")
str(resp.occ)
procnias_f[,"resp.occ"] = 1

#Checando se houve adição da coluna
colnames(procnias_f)
View(procnias_f)

# Agora os registros foram filtrados quanto a inconsistências e potenciais problemas de autocorrelação espacial, estamos prontos para gerar as pseudo-ausencias

#### Formatando os registros de ocorrência para a modelagem no ambiente BIOMOD ####

#Salvando um objeto apenas com o nome da espécie
resp.name <- 'procnias'

# dados de presença da nossa espécie 
is.numeric(procnias_f$resp.occ)
myResp <- as.numeric(procnias_f[,"resp.occ"])

#Criando um objeto apenas com as coordenadas da Araponga e concatenando o valor 1 para todas as coordenadas
myRespXY <- procnias_f[which(myResp==1),c("decimalLongitude", "decimalLatitude")]

# Criando uma camada raster baseada nas camadas ambientais do script 2 para nossa resposta variável 
myResp <- reclassify(subset(biostack2,1,drop=TRUE), c(-Inf,Inf,0))
myResp[cellFromXY(myResp,myRespXY)] <- 1


#Formatando os dados no modelo BIOMOD para depois iniciar o procedimento de modelagem com diferentes algoritmos
#Dado a falta de ausência no conjunto de dados, temos que informar a criação das pseudo-ausências
#Estratégia aleatória
#100 pseudo ausências e 4 sets
bm.procnias100 = BIOMOD_FormatingData(resp.var = resp.occ,
                                      expl.var = biostack2,
                                      resp.xy = myRespXY,
                                      resp.name = resp.name,
                                      PA.nb.rep = 4,
                                      PA.nb.absences = 100, PA.strategy = "random")

#sumário do novo objeto criado
bm.procnias100


#Visualização da configuração espacial das Pseudo Ausências
plot(bm.procnias100) 


#Agora vamos mudar a estratégia de criação das pseudo-ausências para "disk", onde estabelecemos a distância minima e máxima entre as pseudo-ausencias e os registros de ocorrência 


#100 pseudo ausências com estratégia 'disk'
bm.procnias100disk = BIOMOD_FormatingData(resp.var = resp.occ,
                                          expl.var = biostack2,
                                          resp.xy = myRespXY,
                                          resp.name = resp.name,
                                          PA.nb.rep = 4,
                                          PA.nb.absences = 100, PA.strategy = "disk", PA.dist.min = 100000) #distancia em metros
                                          
#sumário do novo objeto criado
bm.procnias100disk
                                          
                                          
                                          
#Visualização da configuração espacial das Pseudo Ausências
plot(bm.procnias100disk) 
                                          
#Agora vamos pré-estabelecer que a nossa área onde serão geradas as pseudo-ausências será fora do envelope climático gerado com o algoritmo SRE (Species Range Envelope) 

bm.procnias100sre = BIOMOD_FormatingData(resp.var = resp.occ,
                                          expl.var = biostack2,
                                          resp.xy = myRespXY,
                                          resp.name = resp.name,
                                          PA.nb.rep = 4,
                                          PA.nb.absences = 100, PA.strategy = "sre", PA.sre.quant = 0.025, na.rm = T)

#sumário do novo objeto criado
bm.procnias100sre                                          
#Visualização da configuração espacial das Pseudo Ausências
plot(bm.procnias100sre)

# Quais foram as diferenças na geração das pseudo ausências com as diferentes estratégias?
# Explore mais opções, alterando o numero de repetições, numero de pseudo-ausencia, distancia minima e quantil do SRE

#Isso é demais!