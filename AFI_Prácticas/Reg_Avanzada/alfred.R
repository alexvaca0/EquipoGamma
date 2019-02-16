#Paquetes----

library(MASS)
library(ISLR)
library(moments)
library(GGally)
library(dplyr)
library(rsq)
library(mlbench)
library(psych)
library(fitdistrplus)
library(lmtest)
library(het.test)
library(sandwich)
library(robustbase)
library(car)
library(nlme)
library(caret)
library(readr)
library(car)
library(ggmap)
library(het.test)
library(lmtest)

#Exploratorio y cambio en las variables----
# id: identificador para cada casa.
# date: fecha de venta de la casa.
# price: precio de venta de la casa.
# bedrooms: número de habitaciones.
# bathrooms: número de baños (puede que haya .5 baños que representarían baños con váter pero no ducha).
# sqft_living: pies cuadrados del interior de una casa.
# sqft_lot: pies cuadrados de terreno de una casa.
# floors: número de plantas de una casa.
# waterfront: variable dummy que vale 1 si la casa está enfrente del mar.
# view: índice de 0 a 4 indicando como de buenas son las vistas.
# condition: índice de 1 a 5 indicando como de buenas son las condiciones de las casas.
# grade: índice de 1 a 13 indicando el grado de construcción y diseño de la casa (de 1-3 mala construcción y diseño, 7 nivel medio de construcción y diseño y de 11-13 muy buena construcción y diseño).
# sqft_above: los pies cuadrados del espacio de la vivienda interior que está por encima del nivel del suelo.
# sqft_basement: pies cuadrados del sotano de una casa.
# yr_built: año de construcción de la casa.
# yr_renovated: año de renovación de la casa.
# zipcode: código postal de la casa.
# lat: latitud.
# long: longitud.
# sqft_living15: pies cuadrados del interior de las casas de los 15 vecinos más próximos.
# sqft_lot15: pies cuadrados del terreno de las casas de los 15 vecinos más próximos.

setwd("C:/Users/aolle/Desktop/regresion avanzada")
data <- read_csv('Data/HomeSalesData.csv')
data$waterfront <- as.factor(data$waterfront)
data$view <- as.factor(data$view)
data$condition <- as.factor(data$condition)

renovado_si_no <- function(year){
  if (is.na(year)) {return(NA)}
  else if (year == 0 ) {return(0)}
  else if(year < 1995) {return(1)}
  else{return(2)}
}

data$yr_renovated <- as.factor(sapply(data$yr_renovated, renovado_si_no))

viejo.nuevo <- function(year){
  if (is.na(year)) {return(NA)}
  else if (year <= 1985 ) {return(0)}
  else if(year >1985 & year < 2010) {return(1)}
  else if(year>= 2010) {return(2)}
}
data$yr_built <- as.factor(sapply(data$yr_built, viejo.nuevo))
data$zipcode <- as.factor(data$zipcode)
data$sqft_basement <- factor(ifelse(data$sqft_basement>0,1,0))
data <- data[,-c(1,2)] #Se elimina id y año


# matriz = vector(mode="numeric", length=0)
# for (i in 1:length(data$zipcode)){
#   for (j in 1:length(data$zipcode)){
#     if (data$zipcode[i] == data$zipcode[j] & !is.na(data$zipcode[i])){
#       matriz = c(matriz, 1)
#     }
#     else{matriz = c(matriz,0)}
#   }
# }
# 
# data$zipcode[1] == data$zipcode[118]

#TRAINING Y TEST SET----

in_train <- createDataPartition(log(data$price), p = 0.75, list = FALSE)  # 75% for training
training <- data[ in_train,]
testing <- data[-in_train,]
nrow(training)
nrow(testing)






#REGRSION SIMPLE----

##Con logaritmos

#1 Usando sqft_living
linFit <- lm(log(price) ~ sqft_living, data=training)
summary(linFit)


### Prediccion

predictions <- exp(predict(linFit, newdata=testing))
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE

#2 Usando zipcode

linFit2 <- lm(log(price) ~ zipcode, data=training)
summary(linFit)

### Prediccion
predictions <- exp(predict(linFit2, newdata=testing))
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE


#Sin logaritmos


#1 Usando sqft_living
linFit <- lm(price ~ sqft_living, data=training)
summary(linFit)


### Prediccion

predictions <- predict(linFit, newdata=testing)
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE

#2 Usando sqft_zipcode

linFit2 <- lm(log(price) ~ zipcode, data=training)
summary(linFit)

### Prediccion
predictions <- predict(linFit2, newdata=testing)
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE


#Modelo Final

modeloaa <- lm(log(price)~sqft_living+zipcode, data= training)
summary(modeloaa)
vif(modeloaa)


#REGRSION MÚLTIPLE----

#Sin logaritmos----

#1 Todas las variables:

rmsl <- lm(formula=price~., data= training)
summary(rmsl)

predictions <- predict(rmsl, newdata=testing)
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE


#Backward selection
step(rmsl, steps = 10000, direction = 'backward')

rmslb <- lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
              floors + waterfront + view + condition + grade + yr_built + 
              yr_renovated + lat + long + sqft_living15 + sqft_lot15, 
            data = training)

summary(rmslb)

predictions <- predict(rmslb, newdata=testing)
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE


#Forward selection

step(rmsl, steps = 10000, direction = 'forward')

rmslf <- lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
              floors + waterfront + view + condition + grade + sqft_above + 
              sqft_basement + yr_built + yr_renovated + lat + 
              long + sqft_living15 + sqft_lot15, data = training)

summary(rmslf)

predictions <- predict(rmslf, newdata=testing)
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE

#Con logaritmos en algunas variables----


data2 <- data
data2[,c(1,4,5,11,18,19)] <- log(data2[,c(1,4,5,11,18,19)]+1)

summary(data2)

in_train2 <- createDataPartition(log(data2$price), p = 0.75, list = FALSE)  # 75% for training
training2 <- data2[ in_train2,]
testing2 <- data2[-in_train2,]
nrow(training)
nrow(testing)

#1 Todas las variables:

rml <- lm(formula= log(price~.), data= training2)
summary(rml)


predictions <- predict(rml, newdata=testing2)
cor(testing2$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing2$price)^2))
RMSE


#Backward selection
step(rml, steps = 10000, direction = 'backward')

rmlb <- lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
             floors + waterfront + view + condition + grade + sqft_above + 
             sqft_basement + yr_built + yr_renovated + lat + long + sqft_living15 + 
             sqft_lot15, data = training2)

summary(rmlb)

predictions <- predict(rmlb, newdata=testing2)
cor(testing2$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing2$price)^2))
RMSE


summary(data$grade)

#Forward selection

step(rml, steps = 10000, direction = 'forward')

rmlf <- lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
             floors + waterfront + view + condition + grade + sqft_above + 
             sqft_basement + yr_built + yr_renovated + lat + long + sqft_living15 + 
             sqft_lot15, data = training2)

summary(rmlf)

predictions <- predict(rmlf, newdata=testing2)
cor(testing2$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing2$price)^2))
RMSE



#Logaritmos sólo en la variable endógena----

rml2 <- lm(formula=log(price)~., data= training)
summary(rml2)

predictions <- exp(predict(rml2, newdata=testing))
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE



#Backward selection
step(rml2, steps = 10000, direction = 'backward')

rml2b <-lm(formula = log(price) ~ bathrooms + sqft_living + sqft_lot + 
             floors + waterfront + view + condition + grade + sqft_above + 
             sqft_basement + yr_built + yr_renovated + zipcode + lat + 
             long + sqft_living15 + sqft_lot15, data = training)

summary(rml2b)

predictions <- exp(predict(rml2b, newdata=testing))
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE



vif(rml2)

#Forward selection

step(rml2, steps = 10000, direction = 'forward')

rml2f <- lm(formula = log(price) ~ bedrooms + bathrooms + sqft_living + 
              sqft_lot + floors + waterfront + view + condition + grade + 
              sqft_above + sqft_basement + yr_built + yr_renovated + zipcode + 
              lat + long + sqft_living15 + sqft_lot15, data = training)


summary(rml2f)

predictions <- exp(predict(rml2f, newdata=testing))
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE

#Modelo----

modelo_predecir <- log(price+exp(1))~sqft_living+sqft_lot+floors+waterfront+view+
  condition+yr_built+yr_renovated+zipcode+sqft_living15+grade

modelo_explicar <- log(price+exp(1))~sqft_living+zipcode



ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 2,
                     number = 5)

test_results <- data.frame(precio = log(testing$price+exp(1)))
observed = log(testing$price+exp(1))

ggcorr(data, label = TRUE)

aa <- regsubsets(log(price)~., nvmax= 2, data = data, really.big=T)# Para ver qué modelo encaja mejor con nvmax variables
summary(aa)
#Regresión robusta----

#Con Caret

rlm_tune <- train(modelo_predecir, data = training, 
                  method = "rlm", 
                  preProc=c('scale', 'center'),
                  trControl = ctrl)

rlm_tune
test_results$rlm <- predict(rlm_tune, testing)
postResample(pred = test_results$rlm,  obs = observed)



#Forward regresion----

for_tune <- train(log(price)~., data = training, 
                  method = "leapForward", 
                  preProc=c('scale', 'center'),
                  tuneGrid = expand.grid(nvmax = 4:10),
                  trControl = ctrl)
for_tune
coef(for_tune$finalModel, for_tune$bestTune$nvmax)
test_results$frw <- predict(for_tune, testing)
postResample(pred = test_results$frw,  obs = test_results$precio)

#Backward regresion----

back_tune <- train(log(price)~., data = training, 
                   method = "leapBackward", 
                   preProc=c('scale', 'center'),
                   tuneGrid = expand.grid(nvmax = 4:10),
                   trControl = ctrl)
back_tune


coef(back_tune$finalModel, back_tune$bestTune$nvmax)
test_results$bw <- predict(back_tune, testing)
postResample(pred = test_results$bw,  obs = test_results$precio)



#Stepwise regression----
step_tune <- train(log(price)~., data = training, 
                   method = "leapSeq", 
                   preProc=c('scale', 'center'),
                   tuneGrid = expand.grid(nvmax = 4:10),
                   trControl = ctrl)
step_tune

coef(step_tune$finalModel, step_tune$bestTune$nvmax)
test_results$seq <- predict(step_tune, testing)
postResample(pred = test_results$seq,  obs = test_results$precio)




#Ridge regression----

ridge_grid <- expand.grid(lambda = seq(0, .1, length = 20))
ridge_tune <- train(modelo_explicar, data = training,
                    method='ridge',
                    preProc=c('scale','center'),
                    tuneGrid = ridge_grid,
                    trControl=ctrl)
ridge_tune
ridge_tune$bestTune
test_results$ridge <- predict(ridge_tune, testing)
postResample(pred = test_results$ridge,  obs = test_results$precio)

#Lasso regression----

lasso_grid <- expand.grid(fraction = seq(.01, 1, length = 20))
lasso_tune <- train(modelo_predecir, data = training,
                    method='lasso',
                    preProc=c('scale','center'),
                    tuneGrid = lasso_grid,
                    trControl=ctrl)

lasso_tune$bestTune
test_results$lasso <- predict(lasso_tune, testing)
postResample(pred = test_results$lasso,  obs = test_results$precio)






#Elastic net----

modelLookup('glmnet')
elastic_grid = expand.grid(alpha = seq(0, .2, 0.01), lambda = seq(0, .1, 0.01))
glmnet_tune <- train(modelo_explicar, data = training,
                     method='glmnet',
                     preProc=c('scale','center'),
                     tuneGrid = elastic_grid,
                     trControl=ctrl)

glmnet_tune$bestTune
test_results$glmnet <- predict(glmnet_tune, testing)
postResample(pred = test_results$glmnet,  obs = test_results$precio)





#Códigos de Machine Learning---- 
#Random Forest----
modelLookup('rf')

colnames(data)

rf_tune <- train(modelo_predecir, data = training,
                 method = "rf",
                 preProc=c('scale','center'),
                 trControl = ctrl,
                 ntree = 100, #Con ntree = 100 tarda 40min
                 tuneGrid = expand.grid(mtry = c(10,20,30,60)), 
                 verbose = FALSE)
plot(rf_tune)
test_results$rf <- predict(rf_tune, testing)
postResample(pred = test_results$rf,  obs = test_results$precio)


#Gradient Boosting----
gbmGrid<-expand.grid(n.trees=seq(100,400,by=100),interaction.depth=seq(1,4,by=1),shrinkage=c(.001,.01,.1), n.minobsinnode=10)
gbm_tune <- train(modelo_explicar, data = training,
                  method = "gbm", # Generalized Boosted Regression Models
                  preProc=c('scale','center'),
                  tuneGrid = gbmGrid,
                  trControl = ctrl, 
                  verbose = FALSE)
plot(gbm_tune)
test_results$gbm <- predict(gbm_tune, testing)
postResample(pred = test_results$gbm,  obs = test_results$precio)

#Extreme gradient boosting----
modelLookup('xgbTree')


xgb_grid_tree = expand.grid(
  nrounds = 1,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = c(0,5),
  min_child_weight = c(1, 10),
  subsample = c(0.5, 1),
  colsample_bytree = c(0.1, 0.4)
)

# too expensive:
xgbTree_tune <- train(modelo_explicar, data = training,
                      method = "xgbTree",
                      preProc=c('scale','center'),
                      tuneGrid = xgb_grid_tree,
                      trControl = ctrl, 
                      verbose = F)
test_results$xgbTree_tune <- predict(xgbTree_tune, testing)
postResample(pred = test_results$xgbTree_tune,  obs = test_results$precio)



#MODELO EXPLICAR----

seatle_map <- get_map(location = c( min(data$long) -0.005 , min(data$lat) -0.005 ,
                                   max(data$long) + 0.005, max(data$lat) + 0.005 ), 
                     zoom = 11, maptype="toner-lite", source="stamen", force = F, color = "bw")


ggmap(seatle_map)+
  geom_point(aes(x=long, y=lat, col = scale(log(price))), data = data)+
  scale_color_gradient2(low = "#009933", midpoint = median(scale(log(data$price))),high = "#cc0000")

#Modelo primigenio 


me <- lm(formula = modelo_explicar, data =data)
summary(me)

#Outliers

obs_inf <- which(abs(studres(me))> qt(0.025, df = 21613-71, lower.tail = FALSE))

length(obs_inf)


me <- lm(formula = modelo_explicar, data =data[-obs_inf,])
summary(me)


#Normalidad de los residuos
hist(me[["residuals"]], nclass = sqrt(21613)) #Los residuos siguen una normal


qqnorm(me[["residuals"]]); qqline(me[["residuals"]],col=2)

descdist(me[["residuals"]])

#Heterocedasticidad

bptest(me)

#Colinealidad

vif(me)





#MODELO A PREDECIR----



#Modelo primigenio

modelo_predecir <- log(price+exp(1))~log(sqft_living+exp(1))+log(sqft_lot+exp(1))+floors+waterfront+view+
  condition+yr_built+yr_renovated+zipcode+log(sqft_living15+exp(1))+grade

mp <- lm(modelo_predecir, data = training)
summary(mp)

vif(mp) #Por debajo de 2


predictions <- exp(predict(mp, newdata=testing))
cor(testing$price, predictions)^2
RMSE <- sqrt(mean((predictions - testing$price)^2))
RMSE

confint(mp, level = 0.90)












