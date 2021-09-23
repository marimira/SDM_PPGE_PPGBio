# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos

################################
# Script 1 - Introdução ao R #
################################

### Operações básicas
###******************

2+2
2 + 2 # soma
2 * 3 # multiplicação
4 / 2 # divisão
10 - 5 # subtração
4 ^ 2 # elevação a potência

### Objetos
###********

X <- 2  # Objeto X é igual a 2 (<- é o mesmo que =)
X
# Lembram que esse símbolo # é para comentários?

### Vetores numéricos
###******************

x <- c(1, 2, 4, 8, 10)
x
# Também pode ser do modo abaixo
c(1, 2, 4, 8, 10) -> x
x
# Ou ainda
x = c(1, 2, 4, 8, 10)
x
# Tente agora com a letra maiúscula abaixo
X
1/x
# Lembrem-se que x é uma sequência de números, logo a expressão acima é identica a de baixo
1/c(1, 2, 4, 8, 10)
y <- c(1.5, 0.002, 3) #Números decimais no R sempre com .
y
# Crie objetos com outros objetos
w <- c(X, 0, x)
w
z <- 1:5 #sequências consecutivas de números com :
z
mean(z)
# Vamos confirmar se o valor acima está correto
media <-(1+2+3+4+5)/5
media # o mesmo valor de mean(z)
# Valores faltantes são representados por NA (not available)
x.missing.data <- c(x, NA)
x.missing.data
# Função para checar se há dados faltantes
is.na(x.missing.data)
# Qual elemento do objeto está faltante?
which(is.na(x.missing.data)) #funções which() e is.na() usadas uma dentro da outra
length(x) #quantos elementos têm o objeto x?
length(x.missing.data) #quantos elementos têm o objeto x.missing.data?
length(which(is.na(x.missing.data)))

### Vetores de caracteres
###**********************

labs <- c("a", "b", "c", "d", "e")
labs
x[1]
labs[5]
sub.labs <- labs[c(2,4)]
sub.labs
# Nomear os elemento de 'x' com o objeto 'labs'
x	
names(x) <- labs
x # viu que agora o [1] sumiu? Isso porque agora os elementos não precisam ser numerados, pois eles têm nomes (a b c d e)
#use a função str() para checar a estrutura do objeto x
str(x) # viu o atributo "names" = "a" "b" "c" "d"?
str(y)
(x[x > 5])
names(x[x > 5])
x[-5]
v<-x[-5]
v
digitos.pares <- x[-c(1,5)] #Dê nomes informativos para seus objetos (sem espaços)
digitos.pares
ls()
#PARE AQUI E ANOTE A RESPOSTA DA PERGUNTA 1 DO ROTEIRO.

### Vetores lógicos
###****************

z
big.numbers <- z >= 4
big.numbers
small.numbers <- z <3
small.numbers
big.numbers & small.numbers
big.numbers | small.numbers
!big.numbers
big.numbers

### Fatores
###********

frutos <- c("presente", "ausente", "ausente", "presente", "presente")
frutos
class(frutos)
frutos <- as.factor(frutos) #tranforma o objeto em 'factor'
class(frutos) #Viu como a classe de frutos agora mudou  para factor?
names(frutos) <- labs
frutos

### Matrizes ou arrays
###*******************

a <- c(1, 2, 4, 8, 10)
a
b <- c(0.1, 0.2, 0.4, 0.8, 1)
b
## Agora, vamos unir esses dois vetores como colunas
m1 <- cbind(a,b)
m1
class(a)
class(m1)
dim(m1) #dimensões da matriz
nrow(m1) #número de linhas da matriz (n row)
ncol(m1) #número de colunas da matriz (n col)
## Agora, vamos unir esses dois vetores como linhas
m2 <- rbind(a,b)
m2
## Vamos colocar nomes na nossa matriz m1
colnames(m1) <- c("col1", "col2")
rownames(m1) <- labs
m1 # Viu que os números antes das linhas sumiram? Isso porque elas agora têm nome (a b c d e)
## Vamos agora selecionar os elementos dess matriz. Queremos o elemento da quarta linha e segunda coluna.
m1[4,2]
# E também podemos selecionar elementos colocando os nomes das linha e colunas como abaixo
m1["d","col2"]
#PARE AQUI E ANOTE A RESPOSTA DA PERGUNTA 2 DO ROTEIRO.

### Data.frame
###***********

plantas <- data.frame(a, b, frutos)
plantas
?paste #Leia as instruções na janela Help ao lado do Console
new.row.names <- paste("sp", 1:5, sep="")
new.row.names
rownames(plantas) <- new.row.names
plantas
colnames(plantas) <- c("petala", "antera", "frutos")
plantas
plantas[ ,1] #todos os elementos da coluna 1
plantas$petala
# Vamos acessar os valores de "sp3"
plantas[3,] #todos os elementos da linha 3
plantas$petala[3] #apenas o terceiro elemento da coluna petala
View(plantas) #Pode fechar a aba depois de vizualizar

### Listas
###*******

lista1 <- list(m1, x, labs)
lista1
names(lista1) <- c("m1", "x", "labs")
lista1 #Veja os nomes seguido de $
# Vamos acessar o terceiro elemento da nossa lista
lista1[[3]]
lista1$labs
# Vamos acessar a primeira linha do primeiro elemento (m1).
lista1[[1]][1,]
lista1$m1[1,]
# Removendo elementos
lista1[-1]
lista1[-c(1,3)]
# Fazendo listas de listas
lista2 <- list(lista1, lista1)
lista2 #Veja que a lista 1 está repetida duas vezes
lista2[[1]][[2]][3] #Acesse o terceiro elemento do segundo componente da primeira lista

### Funções
###********

## Vamos criar uma função:
f <- function() {
  print("Hello world!")
}
## Agora vamos rodar nossa função
f()

### Instalando pacotes
###*******************

install.packages("raster", dependencies=T)
install.packages("biomod2", dependencies=T)
install.packages("rgdal", dependencies=T)
install.packages("sdmpredictors", dependencies=T)
install.packages("maptools", dependencies=T)
install.packages("usdm", dependencies=T)
install.packages("ecospat", dependencies=T)
install.packages("CoordinateCleaner", dependencies=T)
install.packages("rgbif", dependencies=T)
install.packages("spocc", dependencies=T)
install.packages("spThin", dependencies=T)
install.packages("dplyr", dependencies=T)
install.packages("dismo", dependencies=T)
install.packages("gridExtra", dependencies=T)
install.packages("tidyr", dependencies=T)
install.packages("corrplot", dependencies=T)

### Carregando os pacotes
###**********************

library(raster)
library(biomod2)
library(rgdal)
library(sdmpredictors)
library(maptools)
library(usdm)
library(ecospat)
library(CoordinateCleaner)
library(rgbif)
library(spocc)
library(spThin)
library(dplyr)
library(dismo)
library(gridExtra)
library(tidyr)
library(corrplot)

### Ajuda, Help!
###*************

?stack # carregar a ajuda do pacote que vamos usar amanhã
help(stack) # outra forma de carregar a ajuda
?biomod2
??biomod2 # em vez de carregar direto a ajuda do pacote, aqui vamos pesquisar por "biomod2" e encontrar todos os resultados afins
# Ajuda para procurar um pactoe com palavra-chave
help.start() # ajuda geral do R, tente procurar pacotes com a palavra-chave SDM

### Diretório de trabalho
###**********************

setwd("C:/Desktop/my_folder") #Esse caminho está no formato do Windows. Se o seu computador for Mac ou Linux, o formato do caminho é diferente.
# Ou…
my.wd <- "C:/Desktop/my_folder"
setwd(my.wd)
#Tente mudar o caminho entre aspas acima “C:Users/Documents/my_folder” para o caminho da pasta da disciplina no seu computador. Se tiver dificuldade, peça ajuda aos monitores.
# Veja se funcionou usando o comando abaixo para saber em que diretório você está
getwd() # conferir o caminho que escolhemos

### Importando e exportando dados
###*****************************

#Se você conseguiu mudar o caminho acima para a pasta da disciplina, tente ler os pontos da espécie Procnias nudicolis abaixo
read.csv(".Procnias/procnias_nudicollis.csv") #O arquivo 'procnias_nudicollis.csv'está dentro da pasta 'Procnias'

### Plots para visualização
###*************************

plot(x, type="p") #plot de pontos
plot(x, type="l") #plot de linhas
?plot #veja as instruções para o plot genérico no pacote base
#PARE AQUI E ANOTE A RESPOSTA DA PERGUNTA 3 DO ROTEIRO.
barplot(x)
boxplot(x)
hist(c(x, z, y))
hist(c(x, z, y), main = "Histograma de x, y, z", xlab = "")
?par #veja aqui os parâmetros gráficos que podem ser usados nos plots

## FIM do Script 1 ##