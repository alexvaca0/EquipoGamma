####################################
# Práctica de GLM
# Gustavo Eduardo Vargas Núñez
####################################


#### Carga de datos ####
library(data.table)
library(MASS)
library(ggplot2)


Fair <- fread('Fair.csv')

names(Fair)
# Datos de affairs en 1970. Número de engaños en la pareja en función:
# 1.sex             hombre o mujer. Factor
# 2.age             edad
# 3.ym              años de casado
# 4.chil            número de hijos. Factor.
# 5.religious       si es religiosa. Del 1(antirreligiosa) al 5(muy religiosa)
# 6.education       nivel de educación
# 7.occupation      nivel de educación. Del 1 al 7
# 8.rate            cómo es de feliz, del 1 al 5
# 9.nbaffairs       target. Si ha habido infidelidad

str(Fair)
summary(Fair)

sum(is.na(Fair))
# No hay NAs


#### Tratamiento de variables ####

Fair$V1 <- NULL

Fair$sex <- factor(Fair$sex)

Fair$child <- factor(Fair$child)

Fair$religious <- factor(Fair$religious, levels=c("1", "2", "3","4","5"), ordered=TRUE)

Fair$occupation <- factor(Fair$occupation, levels=c("1", "2", "3","4","5","6","7"), ordered=TRUE)

Fair$rate <- factor(Fair$rate, levels=c("1", "2", "3","4","5"), ordered=TRUE)


#### Data partitioning ####

set.seed(123)
train_ind <- sample(seq_len(nrow(Fair)),size=500)

train <- Fair[train_ind, ]
test <- Fair[-train_ind, ]


#### GLM para 'nbaffairs' ####

# Hay que hacer muchos gráficos para entender la relación de nuestra variable con el target
table(Fair$nbaffairs)
qplot(Fair$nbaffairs)  # conteo de número de affairs, pero solo tiene ciertas variables

ggcorr(Fair, label = T)
# vemos mucha relación entre la variable age y ym, lo cual tiene sentido, pero no con el target

# Probamos una poisson sin más (sin ser ceros inflados)
fit.poisson <- glm(nbaffairs ~ .,
                   family = "poisson",
                   data = train)
summary(fit.poisson)
AIC(fit.poisson)



# Como la variable nbaffairs es un conteo, la modelaremos al principio como una poisson. Luego probaremos
# si va mejor una quasi poisson y después, una binomial negativa. 

# Como en la gráfica vemos que los ceros son mucho mayores que el resto de números, usaremos los modelos
# anteriores dentro de un modelo de ceros inflados, donde la bernouilli es el modelo para los ceros.

# ceros inflados con poisson
fit.zip1 <- zeroinfl(nbaffairs ~ .,
                     dist = "poisson",
                     data = train)
summary(fit.zip1)
AIC(fit.zip1) #1302.854, mucho menor que el del poisson

# Probamos un ceros inflados con binomial negativa
fit.zip2 <- zeroinfl(nbaffairs ~ .,
                     dist = "negbin",
                     data = train)
summary(fit.zip2)
AIC(fit.zip2) #1213.876, sigue mejorando





#### GLM para 'rate' ####
table(Fair$rate)
qplot(Fair$rate)
# hay que modelarla como una ordinal

wine.plr <- polr(rate ~ .,
                 data = train)
pr <- profile(wine.plr)



#### Predicción para 'nbaffairs' ####




#### Predicción para 'rate' ####







# nbaffairs es fácil sacar qué distribución es
# Para rate es un poco más difícil saber qué tipo de distribución es. Pero seguro que es una poisson

# Note: variable 'rate' is ordinal, hence ordinal GLM is recommended, like MASS:polr

# For both models, provide full interpretation and insights (in terms of coefficients and diagnosis)

