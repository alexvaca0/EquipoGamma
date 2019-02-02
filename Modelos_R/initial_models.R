#This script is used to try all regression-type models over the data initially. 
#the output will be a data frame (later passed to csv) in which the RMSE, MAE, and other error measures will
#be taken for each method. 

library(caret)
library(caretEnsemble)
library(doParallel)
library(dplyr)

library(data.table)
library(ggplot2)


models <- unique(modelLookup()[modelLookup()$forReg,c(1)])



df <-  fread('Modelar_UH2019.txt', encoding = 'Latin-1')

df$HY_id <- as.character(df$HY_id)

df$HY_provincia <- as.factor(df$HY_provincia)

df$HY_metros_totales <- log1p(df$HY_metros_totales)

df[is.na(df$HY_metros_utiles), 'HY_metros_utiles'] <- df$HY_metros_totales[is.na(df$HY_metros_utiles)]

df$HY_metros_utiles <- log1p(df$HY_metros_utiles)

df$HY_num_banos <- log1p(df$HY_num_banos)




myfunc <- function(x) {
  
  is.numeric(x) | is.factor(x)
}


cols <- unlist(lapply(df, is.numeric))

cols2 <- unlist(lapply(df, is.factor))
  
df_numeric <- data.frame(df)[ , cols | cols2]

df_numeric$HY_antiguedad <- NULL

df_numeric$IDEA_ind_elasticidad <- NULL

df_numeric$HY_precio_anterior[is.na(df_numeric$HY_precio_anterior)] <- df_numeric$HY_precio[is.na(df_numeric$HY_precio_anterior)]


no_rows_ruidosas <- function(df) {
  
  nc <- ncol(df)
  
  df_new <- df
  
  for(i in 1:nrow(df)) {
    
    if((sum(is.na(df[i,])) / nc) > 0.2 ) {
      
      df_new <- df_new[-i, ]
      
    } else {
      
      next
      
    }
  }
  
  return(df_new)
}

numerico_sin_ruido <-  no_rows_ruidosas(df_numeric)

numerico_sin_ruido <- numerico_sin_ruido[complete.cases(numerico_sin_ruido), ]

X <- numerico_sin_ruido[ , -ncol(numerico_sin_ruido)]

y <- numerico_sin_ruido[ , 'TARGET']


set.seed(123)
part.index <- createDataPartition(y, 
                                  p = 0.8,                         
                                  list = FALSE)
X_train <- X[part.index, ]
X_test <- X[-part.index, ]
y_train <- y[part.index]
y_test <- y[-part.index]


set.seed(123)

registerDoParallel(4)
getDoParWorkers()

control <- trainControl(method = "cv", # for “cross-validation”
                           number = 5, # number of k-folds
                           savePredictions = "final",
                           allowParallel = TRUE)


set.seed(222)
model_list <- caretList(X_train,
                        y_train,
                        trControl = control,
                        methodList = models,
                        tuneList = NULL,
                        continue_on_fail = TRUE, 
                        preProcess = c("center","scale"))


models_results <- data.frame(model = models)


min_error <- c()

for (i in 1:length(model_list)) {
  
  if(all(!is.na(model_list[[i]]$results$RMSE))) {
    
    min_error <- c(min_error, min(model_list[[i]]$results$RMSE, na.rm=T))
    
  } else {
    
    min_error <- c(min_error, NA)
    
  }
  
}

min_mae <- c()

for (i in 1:length(model_list)) {
  
  if(all(!is.na(model_list[[i]]$results$MAE))) {
    
    min_mae <- c(min_mae, min(model_list[[i]]$results$MAE, na.rm=T))
    
  } else {
    
    min_mae <- c(min_mae, NA)
    
  }
  
}

models_results$MAE <- min_mae


write.csv(models_results, 'preliminary_models_resylts.csv', sep = ',', fileEncoding = 'utf-8')










