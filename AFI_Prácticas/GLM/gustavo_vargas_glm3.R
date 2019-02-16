####################################
# Práctica de GLM
# Gustavo Eduardo Vargas Núñez
####################################


#### Carga de datos ####
library(data.table)
library(dplyr)
library(ggplot2)
library(GGally)
library(MASS)
library(pscl)
library(caret)
library(effects)


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

# Partimos el dataset en train y test. Vamos a hacer en análisis con train, y pasaremos
# a usar el test solo en el apartado final, para predicción.

set.seed(123)
train_ind <- sample(seq_len(nrow(Fair)),size=500)

ftrain <- Fair[train_ind, ]
ftest <- Fair[-train_ind, ]



#### Análisis univariante ####

####
# ftrain$sex ftrain[,1]
####

qplot(ftrain$sex)
summary(ftrain$sex)
# Hay poco menos de hombres, pero bien balanceado

# La infidelidad media tampoco parece ser distinta por sexos
ftrain %>%
  group_by(sex) %>% 
  summarise(media_nbaffairs = mean(nbaffairs)) %>% 
  ggplot(aes(x = sex,
             y=media_nbaffairs,
             fill=sex)) +
  geom_histogram(stat='identity')

# Hay más mujeres felices que hombres (hay que pasarlo a %)
ftrain %>% 
  ggplot(aes(x = rate,fill=sex))+
  geom_bar(position = "dodge")+
  facet_grid(~sex)



####
# ftrain$age ftrain[,2]
####

ftrain %>% 
  ggplot(aes(age))+
  geom_histogram(binwidth = 5, col='blue',fill='red',alpha=.2)

table(ftrain$age)
summary(ftrain$age)
# La moda es 27, mediana 32, media 32.59

# En general la gente no es infiel. Y los que lo son, con la edad lo son menos.
ftrain %>%
  ggplot(aes(x = factor(nbaffairs), y = factor(age))) +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')


# En general, la gente es bastante feliz en su matrimonio. Con el tiempo va empeorando.
ftrain %>%
  ggplot(aes(x = rate, y = factor(age))) +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')


####
# ftrain$ym ftrain[,3]
####


qplot(ftrain$ym)
# Gran parte de la muestra llevan casados quince años o más

# La mayor parte de casados no han tenido infidelidades (estaría bien ponerlo en % según cada ym)
ftrain %>%
  ggplot(aes(x = ym, y = nbaffairs)) +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')


# Similar al de la variable anterior. La mayoría son felices, aunque una mayor cantidad de felices son
# los que llevan mucho tiempo casados.
ftrain %>%
  ggplot(aes(x = ym, y = rate)) +
  geom_count(aes(color = ..n.., size = ..n..))


####
# ftrain$child ftrain[,4]
####


qplot(ftrain$child)
summary(ftrain$child)
# Hay poco menos de hombres, pero bien balanceado

# La gente con hijos, de media, es más infiel
ftrain %>%
  group_by(child) %>% 
  summarise(media_nbaffairs = mean(ftrain$nbaffairs)) %>% 
  ggplot(aes(x = child,
             y=media_nbaffairs,
             fill=child)) +
  geom_histogram(stat='identity')

# Hay más gente con hijos. El efecto es complejo. Los que no tienen hijos tienden a puntuar con un 5,
# sin embargo los que tienen hijos tienden más al 4 y 5.
ftrain %>% 
  ggplot(aes(x = rate,fill=child))+
  geom_bar(position = "dodge")+
  facet_grid(~child)

####
# ftrain$religious ftrain[,5]
####

qplot(ftrain$religious)
# La gente se concentra en la moderación (ni extremadamente religiosa ni antirreligiosa)

# ¿Las parejas religiosas son infieles?
# La distribución de infidelidad parece igual en los mismos niveles de religiosidad
ftrain %>% 
  ggplot(aes(x = religious, y = nbaffairs))  +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')


# Los que son religiosos, pero sin ser extremos (religious=4) parecen ser los que mejor puntúan su
# relación.
ftrain %>%
  ggplot(aes(x = religious, y = rate))  +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')


####
# ftrain$education ftrain[,6]
####

qplot(ftrain$education)

table(ftrain$education)

# Poca gente comete infidelidades, pero el máximo nivel educativo que no las comete está alrededor
# de 1.
ftrain %>%
  ggplot(aes(x = education, y = nbaffairs)) +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')

ftrain %>%
  ggplot(aes(x = education, y = rate)) +
  geom_count(aes(color = ..n.., size = ..n..)) +
  guides(color = 'legend')


####
# ftrain$occupation ftrain[,7]
####

qplot(ftrain$occupation)
# Tiene dos máximos relativos

ftrain %>% 
  ggplot(aes(x = occupation, y = nbaffairs))  +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')

ftrain %>%
  ggplot(aes(x = occupation, y = rate))  +
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')


####
# ftrain$rate ftrain[,8]
####

qplot(ftrain$rate)
# No hay outliers y los valores tienen sentido. Son %

summary(ftrain$rate)

ftrain %>%
  ggplot(aes(x = ftrain$rate, y = nbaffairs))+
  geom_count(aes(color = ..n.., size = ..n..))+
  guides(color = 'legend')


####
# ftrain$nbaffairs ftrain[,9]
####

qplot(ftrain$nbaffairs)

table(ftrain$nbaffairs) 

####
# correlación entre variables
####

ggpairs(ftrain)

ggcorr(ftrain, label = T)
# vemos mucha relación entre la variable age y ym, lo cual tiene sentido, pero no con el target



#### GLM para 'nbaffairs' ####

# Probamos una poisson, sin más, para tener una referencia del AIC
fit.poisson <- glm(nbaffairs ~ .,
                   family = "poisson",
                   data = ftrain)
summary(fit.poisson)
AIC(fit.poisson) #2413.53

# Como la variable nbaffairs es un conteo, la modelaremos al principio como una poisson. Luego probaremos
# si va mejor una quasi poisson y después, una binomial negativa. 

# Como en la gráfica vemos que los ceros son mucho mayores que el resto de números, usaremos los modelos
# anteriores dentro de un modelo de ceros inflados, donde la bernouilli es el modelo para los ceros.

# Ceros inflados seguro
fit.zip1 <- zeroinfl(nbaffairs ~ .,
                     dist = "poisson",
                     data = ftrain)
summary(fit.zip1)
AIC(fit.zip1) #1302.854, mucho menor que el del poisson

# (Aquí tenemos que poner un ejemplo de interpretación de un beta)

# Probamos un ceros inflados con binomial negativa
fit.zip2 <- zeroinfl(nbaffairs ~ .,
                     dist = "negbin",
                     data = ftrain)
summary(fit.zip2)
AIC(fit.zip2) #1213.876, sigue mejorando

# Para comparar modelos, usamos los AIC
AIC.nbaffairs <- c(AIC(fit.poisson), AIC(fit.zip1), AIC(fit.zip2))
AIC.nbaffairs

#### GLM para 'rate' ####

fair.plr <- polr(rate ~ .,
                 data = ftrain)
AIC(fair.plr) #1318.96

summary(fair.plr, digits = 3)

plot(allEffects(fair.plr), rescale.axis=FALSE, multiline=TRUE, rug=FALSE, main="")
# (Comentar la gráfica)



#### Predicción para 'nbaffairs' ####

# Con predict(fit.lm, type = "response") obtengo una línea de predicciones
# Aquí las comparo con el target actual. Deberían estar en la línea de 45º

# Y ahora lo hago con el zip2, que era un modelo de ceros inflados
plot(ftrain$nbaffairs,
     predict(fit.zip2, type = "response"),
     xlab="actual",ylab="predicted")
abline(a=0,b=1);
cor(ftrain$nbaffairs,
    predict(fit.zip2, type = "response"))^2


#Vamos a mirar hallar los errores de las predicciones
pred.na = predict(fit.zip2,    
               newdata=ftest,
               type = "response")

rmse = sqrt(mean((pred.na-ftest$nbaffairs)^2))
rmse #2.99

# unconditional error. Desviación típica
sd(ftest$nbaffairs)

# R2 in testing set. El R2
cor(pred.na, ftest$nbaffairs)^2



#### Predicción para 'rate' ####

pred.ra <- predict(fair.plr, ftest, type = "class")

m1.pred <- predict(fitpl, newdata = rate_test,type="class")

confusionMatrix(reference = ftest$rate, data = pred.ra)