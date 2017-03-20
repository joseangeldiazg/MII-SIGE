#Cargamos paquetes

require(plyr)
require(ggplot2)
require(reshape2)

#Cargar datos

adult.training <- read.table(file = "adult.training.csv", sep = ',', header = TRUE)
head(adult.training)
str(adult.training)

adult.test <- read.table(file = "adult.test.csv", sep = ',', header = TRUE)
head(adult.test)
str(adult.test)

#Con Rbind pegamos ambos datasets

adult <- rbind(adult.training, adult.test)

#Selección de datos

adult.reduced <- head(adult, n = 500)
adult[adult$Workclass == "Private",]
adult[adult$Age > 45,]
unique(adult.reduced["Marital.status"])

# Combinación de datos
status.eng <- c(unique(adult.reduced["Marital.status"]))
status.sp  <- c(Marital.status.sp = "Casado/a", "No casado/a", "Divorciado/a", "Separado/a", "Viudo/a", "Casado/a, conyuge ausente")
status.transl <- data.frame(status.eng, status.sp)
adult.transl <- join(x = adult, y = status.transl, by = c("Marital.status"))

# Cálculo de campos derivados

adult.transl$birthYear <- with(adult.transl, 2017-Age)

#Agrupar datos: A partir de un data set tendremos otros donde las filas tendran 
#la funcion que le pasemos agrupada. Por ejemplo contar, sumar...

adult.count.byage <- ddply(adult, c("Age"), function(x) {count(x$Class)})


adult.avgage.byclass <- ddply(adult, c("Class"), function(x) {mean(x$Age)})

#Esto nos ofrece información como que la media de edad de los que ganan mas de
#50000 es mayor que la media de los que ganan menos. 


#Agrupar datos

adult.count.byage <- ddply(adult, c("Age"), function(x) {count(x$Class)})
adult.avgage.byclass <- ddply(adult, c("Class"), function(x) {mean(x$Age)})

#VISUALIZACIÓN GRÁFICA

# 1) histogramas
qplot(data = adult, x = Age)

ggplot(data = adult, aes(x = Age)) + geom_histogram(binwidth = 1) 

#ggplot, datos que queremos utilizar y que vamos a usar en los ejes.Bindwith nos
#indica el ancho de cada una de las particiones del eje x, es decir 1 indica por
#cada año, si metemos binwith de 50 indicará que nos contará cuantos tienen entre
#0 y 50 y cuantos entre 50 y el máximo.

ggplot(data = adult, aes(x = Age, fill=Class)) + geom_histogram(binwidth = 1) 

#Añadiendo fill nos ofrece la distribución de los que ganan más o menos de 50000. 

ggplot(data = adult, aes(x = Age, fill=Class)) + geom_histogram(binwidth = 1) + facet_wrap(~Class)

# Con facet_wrap partimos los dos gráficos en dos. 

ggplot(data = adult, aes(x = Age, colour = Class)) +
  geom_freqpoly(binwidth = 1)  

# Con freqpoly podremos obtener la línea de frecuencias. 

ggplot(data = adult, aes(x = Age, colour = Class)) +
  geom_freqpoly(binwidth = 1)  + facet_wrap(~Sex)

#Podemos ir añadiendo elementos como por ejemplo el sexo, que nos muestra 
#la gran diferencia entre hombres y mujeres. 

#2) Diagrama de clases. 

ggplot(data = adult, aes(x = Class, fill = Class)) +
  geom_bar() 

adult.count.byrace <- ddply(adult, c("Race"), function(x) {count(x$Class)})
colnames(adult.count.byrace) <- c("Race", "Salary", "Count")
adult.count.byrace.twocol <- dcast(adult.count.byrace, Race ~ Salary)
adult.count.byrace.twocol$Total <- adult.count.byrace.twocol$`<=50K` + adult.count.byrace.twocol$`>50K`
adult.count.byrace.twocol$`<=50K` <- adult.count.byrace.twocol$`<=50K` / adult.count.byrace.twocol$Total * 100
adult.count.byrace.twocol$`>50K` <- adult.count.byrace.twocol$`>50K` / adult.count.byrace.twocol$Total * 100
adult.count.byrace.twocol$Total <- NULL

adult.count.byrace.perc <- melt(adult.count.byrace.twocol, id.vars = c('Race'), variable.name = 'Salary', value.name = 'Perc')
ggplot(data = adult.count.byrace.perc, aes(x = Race, y = Perc, fill = Salary)) +
  geom_bar(stat = "identity") 


#PREPARACION DE DATOS

# 1) Eiminar education-num
#La codificación númercia del nivel de educación no es útil, no interesa.
#Obtengo todas las filas y elimino las columnas que cumplan los valores dados
# es decir, las columnas que sean Education.num o status_sp

adult <- adult[, !(names(adult) %in% c("Education.num", "status_sp"))]


# 2) Reducir el número de variables con muchos valores (education)
#Vamos a unir las categorias de educación de alguna manera más sencilla
#Todos los valores de educacion "12th", "11th", "10th", "9th", "7th-8th",
# "5th-6th", "1st-4th", "Preschool")] <- "Unfinished" ) se les pasará a
#sin acabar. 

ggplot(data = adult, aes(x = Education, fill = Education)) + geom_bar() 
adult$Education <- as.character(adult$Education)
adult <- within(adult, Education[Education %in% c("12th", "11th", "10th", "9th", "7th-8th", "5th-6th", "1st-4th", "Preschool")] <- "Unfinished" )
adult$Education <- as.factor(adult$Education)
ggplot(data = adult, aes(x = Education, fill = Education)) + geom_bar() 

# 3) Detectar y eliminar outliers (age, hours per week)

#Todo aquel que se desvia más de tres desviaciones de la media
#No lo uso para la prediccion ya que es muy probable que estemos ante un 
#outlier o ruido


ggplot(data = adult, aes(x = Age, fill = Class)) + geom_histogram(binwidth = 1) 
age_sd <- sd(adult$Age)
age_mean <- mean(adult$Age)
age_upper <- round(age_mean + 3 * age_sd, 0)
age_lower <- 17
adult <- adult[adult$Age >= age_lower & adult$Age <= age_upper, ]
ggplot(data = adult, aes(x = Age, fill = Class)) + geom_histogram(binwidth = 1) 


#La interpretacion del gráfico puede estar en que todos los valores por encima de 
#80 los hemos quitado ya que son ruidos. 


#4) Valores perdidos
#Para eliminar valores perdidos podemos eliminarlos con 

adult<- adult[apply(adult,1,function(row) !any (row %in% c("?"))),]

# 5) Construir predictor
require(rpart)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
require(caret)

sample_size <- floor(0.75 * nrow(adult))
set.seed(123)
train_ind <- sample(seq_len(nrow(adult)), size = sample_size)
adult.training   <- adult[train_ind, ]
adult.validation <- adult[-train_ind, ]

class_tree <- rpart(Class ~ Age + Workclass + Fnlwgt + Education + Marital.status + Occupation + Relationship + Race + Sex + Capital.gain + Capital.loss + Hours.per.week + Native.country, 
                    data=adult.training,
                    method="class")
#plot(predictor)
#text(predictor)
fancyRpartPlot(class_tree)

prediction.validation <- predict(class_tree, adult.validation, type = "class")
prediction.training   <- predict(class_tree, adult.training, type = "class")
prediction <- rbind(cbind(adult.training, Prediction=prediction.training), cbind(adult.validation, Prediction=prediction.validation))
confTable <- table(prediction$Class, prediction$Prediction)


# Realizar imputación de valores perdidos, 
# Por defecto: predictive mean matching (pmm), m = 5 (número de imputaciones)
# En el dataset, hay valores perdidos en Native.country, Workclass, Occupation
# imp <- mice(adult.training)
imp <- mice(data = adult.training, method = c("", "polyreg", "", "", "", "", "cart", "", "", "", "", "", "", "sample", ""), m = 3)
str(imp)

# Analizar datasets con valores imputados
complete(imp)   # complete(imp, 2)
require(lattice)
plot(imp)
xyplot(imp, Workclass ~ Age + Marital.status)

#Para ver la distribucion de las imputaciones en cada clase
# los valores azules indican no imputados.

stripplot(imp, pch = 20, cex = 1.2)


# Especificar predictores
pred <- imp$predictorMatrix
pred[,"Age"] <- 0
pred[2, 1] <- 0
# imp <- mice(adult.training, predictorMatrix = pred)

# Recomendaciones: reducir variables con muchas categorías, estudiar correlaciones
quickpred(adult.training)

# imp <- mice(adult.training, predictorMatrix = pred)

# Recomendaciones: reducir variables con muchas categorías, estudiar correlaciones
quickpred(adult.training)

# Usar datasets con conjuntos imputados
# -- usando un dataset con imputaciones cualquiera
require(caret)
training <- complete(imp)
rf_1 <- caret::train(Class ~ . , data = training, method = "rf")
training <- complete(imp, 2)
rf_2 <- caret::train(Class ~ . , data = training, method = "rf")
training <- complete(imp, 3)
rf_3 <- caret::train(Class ~ . , data = training, method = "rf")
training <- adult.training[apply(adult.training, 1, function(row) !any (row %in% c(NA))), ]
rf <- caret::train(Class ~ . , data = training, method = "rf")

predictions1 <- predict(rf1, newdata = adult.test)
predictions2 <- predict(rf2, newdata = adult.test)
predictions3 <- predict(rf3, newdata = adult.test)
predictions4 <- predict(rf4, newdata = adult.test)
write.csv(predictions1, file = "predictions1.csv")
write.csv(predictions2, file = "predictions2.csv")
write.csv(predictions3, file = "predictions3.csv")
write.csv(predictions4, file = "predictions4.csv")
write.csv(predictions, file = "predictions.csv")
