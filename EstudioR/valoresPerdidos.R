# Leer datos
adult.training <- read.csv("adult.training.csv", header = TRUE, sep = ",")
adult.test <- read.csv("adult.test.csv", header = TRUE, sep = ",")

# Reemplazar valores '?' por NA
adult.training[adult.training == '?'] <- NA

# Reducir el tamaño del dataset
adult.training <- head(adult.training, n = 500)

# Listar número de valores perdidos, según combinaciones de variables
require(mice)
md.pattern(adult.training)

# Listar número de valores perdidos, por pares de variables
# sirve para ver las relaciones de variables con valores perdidos 
# Cuando una clase tiene valor perdido y la otra tambien
md.pairs(adult.training)


# Visualizar datasets con valores perdidos con paquete VIM
require(VIM)
aggr(adult.training, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(adult.training), cex.axis=.7, gap=3, ylab=c("Histogram of missing data", "Pattern"))


marginplot(adult.training[,c("Workclass","Native.country")], col=c("blue","red","orange"), cex=1.5, cex.lab=1.5, cex.numbers=1.3, pch=19)

pbox(adult.training, pos=1, int=FALSE, cex=1.2)


#Otra manera de ver los valores perdidos puede ser la funcion mismap

require(Amelia)

missmap(adult.training, "Perdidos vs No Perdidos")
missmap(adult.test, "Perdidos vs No Perdidos")




