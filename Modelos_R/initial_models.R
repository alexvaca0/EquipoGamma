#This script is used to try all regression-type models over the data initially. 
#the output will be a data frame (later passed to csv) in which the RMSE, MAE, and other error measures will
#be taken for each method. 

library(caret)

models <- unique(modelLookup()[modelLookup()$forReg,c(1)])


df <- read.csv('datos.csv', fileEncoding = 'utf-8', stringsAsFactors = F)

X <- df[ , - #numero de fila de la var obj]
           ];

y <- df[ , #num fila var obj]
        ]

control <- trainControl(method="repeatedcv", number=5, repeats=1)


new_df <- 



