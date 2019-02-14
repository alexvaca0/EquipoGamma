####################################
# Práctica de GLM
# Gustavo Eduardo Vargas Núñez
####################################

library(data.table)
library(MASS)

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


#### GLM para 'rate' ####




#### Predicción para 'nbaffairs' ####




#### Predicción para 'rate' ####







# nbaffairs es fácil sacar qué distribución es
# Para rate es un poco más difícil saber qué tipo de distribución es

# Note: variable 'rate' is ordinal, hence ordinal GLM is recommended, like MASS:polr

# For both models, provide full interpretation and insights (in terms of coefficients and diagnosis)

