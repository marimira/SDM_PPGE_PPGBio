# IBE 875 - Modelagem de Distribuição de Espécies
# PPGE/PPGBio
# Professores: Rodrigo Tardin, Maria Lucia Lorini, Mariana Vasconcellos
# Script 1 - Introduction to R

##Inicio do script

2+2
2 + 2
2 * 3
4 / 2
10 - 5
4 ^ 2
X <- 2  #Objeto X é igual a 2 (<- é o mesmo que =)
# Lembram que esse símbolo # é para comentários?
x <- c(1, 2, 4, 8, 10)
## Também pode ser do modo abaixo
c(1, 2, 4, 8, 10) -> x
### Ou ainda
x = c(1, 2, 4, 8, 10)
x
## Tente agora com a letra maiúscula abaixo
X
1/x
1/c(1, 2, 4, 8, 10) #Números decimais sempre com .
X+1*2
# Crie objetos com outros objetos
y <- c(X, 0, x)
y
z <- 1:5
# Valores faltantes são representados por NA
x.missing.data <- c(x, NA)
x.missing.data
# Checando se há missing data
is.na(x.missing.data)
# Qual elemento do objeto está faltante?
which(is.na(x.missing.data))
length(x)
length(x.missing.data)
length(which(is.na(x.missing.data)))
labs <- c("a", "b", "c", "d", "e")
labs
x[1]
labs[5]
sub.labs <- labs[c(2,4)]
sub.labs
x	
names(x) <- labs
x
names(x[x > 5])
x[-5]
digitos.pares <- x[-c(1,5)] #Dê nomes informativos para seus objetos (sem espaços)
z	
big.numbers <- z > 4
s.numbers <- z <3
big.numbers & s.numbers
big.numbers | s.numbers
! big.numbers
frutos <- c("presente", "ausente", "ausente", "presente", "presente")
frutos
class(frutos)
cores <- as.factor(frutos)
class(frutos)
names(frutos) <- labs
frutos
a <- c(1, 2, 4, 8, 10)
b <- c(0.1, 0.2, 0.4, 0.8, 1)
## Agora, vamos unir esses dois vetores como colunas
m1 <- cbind(x,y)
m1
class(a)
class(m1)
dim(m1) #dimensões da matriz
nrow(m1)
ncol(m1)
## Agora, vamos unir esses dois vetores como linhas
m2 <- rbind(x,y)
m2
## Vamos colocar nomes na nossa matriz m1
colnames(m1) <- c("col1", "col2")
rownames(m1) <- labs
m1[4,2]
plantas <- data.frame(a, b, frutos)
plantas
?paste
new.row.names <- paste("sp", 1:5, sep="")
new.row.names
rownames(plantas) <- new.row.names
plantas
colnames(plantas) <- c("petala", "antera", "frutos")
plantas
plantas[ ,1] #coluna 1
plantas$petala
# Vamos acessar os valores de "sp3"
plantas[3,] #linha 3
plantas$petala[3]
lista <- list(m1, x, labs)
lista
names(lista) <- c("m1", "x", "labs")
lista
lista[[3]]
lista$labs
lista[[1]][1,]
lista$m1[1,]
lista[-1]
lista[-c(1,3)]
lista2 <- list(lista, lista)
lista2
lista2[[1]][[2]][3] #Acesse o terceiro elemento do segundo componente da primeira lista
## Vamos criar uma função:
f <- function() {
  print("Hello world!")
}
## Agora vamos roda nossa função
f()
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
?stack
help(stack)
?biomod2
??biomod2
help.start()
setwd("C:/Desktop/my_folder") #Esse caminho está no formato do Windows. Se o seu computador for Mac ou Linux,  o formato do caminho é diferente.
# Ou…
my.wd <- "C:/Desktop/my_folder"
setwd(my.md)

#Plots para visualização
barplot(x)
boxplot(x)
hist(c(x, y, x))

#FIM#