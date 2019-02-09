getwd()
setwd("~/00/EquipoGamma/datos_originales")
setwd("C:/Users/gvargas/00 - Máster Data Science AFI/21 - Análisis de Series Temporales y Datos de Panel/Series Temporales")
setwd("C:/Users/gvargas/Downloads/Datasets_Reto_Modelling_UH2019/Datasets_Reto_Modelling_UH2019")
## EXPLORATORIO VARIABLES
library(data.table)
library(dplyr)
library(ggplot2)
library(outliers)

rm(list=ls())
rm(df)

# df <- fread('Estimar_UH2019.txt', encoding = 'UTF-8')
df <- fread('Modelar_UH2019.txt', encoding = 'UTF-8')

#df1 es el dataframe que iré modificando
df1 <- df


# Para Álex
names(df)[1:25]

# Para Gustavo
names(df)[25:52]

str(df[25:52])


minmaxscaler = function(x){
  new = ((x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T)))
  return(new)
}


## Recordemos que los valores IDEA están relacionados con la zona,
## no del propio inmueble



############################
# df$IDEA_pc_1990_99 df[,25]
############################

boxplot(df$IDEA_pc_1990_99)
# No hay outliers y los valores tienen sentido. Son %

hist(df$IDEA_pc_1990_99)

summary(df$IDEA_pc_1990_99)
# Mediana 0.14  Media 0.1494
# 2710 NA. Casi un 30% de NA

#% de NA: 27,21%
sum(is.na(df$IDEA_pc_1990_99))/nrow(df)

table(df$IDEA_pc_1990_99) # 56 entradas tienen el valor 0 (#cero#)

# No hay mucha relación entre la construcción de la década de los 90 y el target
df1 %>%
  ggplot(aes(x = IDEA_pc_1990_99, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_1990_99 <- ifelse(is.na(df$IDEA_pc_1990_99), "YES", "NO")
sum(df1$IDEA_pc_1990_99 == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_1990_99, y = TARGET, fill=IDEA_pc_1990_99)) +
  geom_bar(stat="summary", fun.y="mean")
# No hay diferencia entre si tiene o no NAs -> eliminamos la variable



############################
# df$IDEA_pc_2000_10 df[,26]
############################

boxplot(df$IDEA_pc_2000_10)

hist(df$IDEA_pc_2000_10)

summary(df$IDEA_pc_2000_10)
# Mediana 0.30  Media 0.3287
# 2710 NA. Casi un 30% de NA

#% de NA: 27,21%
sum(is.na(df$IDEA_pc_2000_10))/nrow(df)

# No hay mucha relación entre la construcción de la década de los 2000 y el target
df1 %>%
  ggplot(aes(x = IDEA_pc_2000_10, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_2000_10 <- ifelse(is.na(df$IDEA_pc_2000_10), "YES", "NO")
sum(df1$IDEA_pc_2000_10 == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_2000_10, y = TARGET, fill=IDEA_pc_2000_10)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable



############################
# df$IDEA_pc_comercio df[,27] #Eliiminar
############################

boxplot(df$IDEA_pc_comercio)

hist(df$IDEA_pc_comercio)

summary(df$IDEA_pc_comercio)
# Mediana 0.02  Media 0.0215
# 2710 NA. Casi un 30% de NA

# No hay mucha relación con el IDEA_pc_comercio con el target, si acaso es ligeramente negativa
df1 %>%
  ggplot(aes(x = IDEA_pc_comercio, y = TARGET)) +
  geom_point(alpha=0.1)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_comercio <- ifelse(is.na(df$IDEA_pc_comercio), "YES", "NO")
sum(df1$IDEA_pc_comercio == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_comercio, y = TARGET, fill=IDEA_pc_comercio)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable

############################
# df$IDEA_pc_industria df[,28]
############################

boxplot(df$IDEA_pc_industria)

hist(df$IDEA_pc_industria)

summary(df$IDEA_pc_industria)
# Mediana 0.00  Media 0.0075
# 2710 NA. Casi un 30% de NA

table(df$IDEA_pc_industria)
# 0.00    3842
# 0.01    2523
# 0.02    474

df %>% filter(IDEA_pc_industria < 0.05) %>% 
  ggplot(aes(x = IDEA_pc_industria, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_industria <- ifelse(is.na(df$IDEA_pc_industria), "YES", "NO")
sum(df1$IDEA_pc_industria == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_industria, y = TARGET, fill=IDEA_pc_industria)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable

############################
# df$IDEA_pc_oficina df[,29]
############################

boxplot(df$IDEA_pc_oficina)

hist(df$IDEA_pc_oficina)

summary(df$IDEA_pc_oficina)
# Mediana 0.01  Media 0.0073
# 2710 NA. Casi un 30% de NA

table(df$IDEA_pc_oficina)
# 0.00    3457
# 0.01    2993
# 0.02    526

df %>%
  ggplot(aes(x = IDEA_pc_oficina, y = TARGET, col=HY_tipo)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)



# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_oficina <- ifelse(is.na(df$IDEA_pc_oficina), "YES", "NO")
sum(df1$IDEA_pc_oficina == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_oficina, y = TARGET, fill=IDEA_pc_oficina)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable
# pendiente

############################
# df$IDEA_pc_otros df[,30]
############################

boxplot(df$IDEA_pc_otros)

hist(df$IDEA_pc_otros)

summary(df$IDEA_pc_otros)
# Mediana 0.04  Media 0.0546
# 2710 NA. Casi un 30% de NA

table(df$IDEA_pc_otros)
# casi todo está entre 0.01 y 0.05

# relación mucho más débil que el resto
df %>%
  ggplot(aes(x = IDEA_pc_otros, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_otros <- ifelse(is.na(df$IDEA_pc_otros), "YES", "NO")
sum(df1$IDEA_pc_otros == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_otros, y = TARGET, fill=IDEA_pc_otros)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_pc_residencial df[,31]
############################

boxplot(df$IDEA_pc_residencial)

hist(df$IDEA_pc_residencial)

summary(df$IDEA_pc_residencial)
# Mediana 0.50  Media 0.4996
# Esta sí es media simétrica
# 2710 NA. Casi un 30% de NA

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_pc_residencial, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_residencial <- ifelse(is.na(df$IDEA_pc_residencial), "YES", "NO")
sum(df1$IDEA_pc_residencial == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_residencial, y = TARGET, fill=IDEA_pc_residencial)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_pc_trast_parking df[,32]
############################

boxplot(df$IDEA_pc_trast_parking)

hist(df$IDEA_pc_trast_parking)

summary(df$IDEA_pc_trast_parking)
# Mediana 0.42  Media 0.4079
# 2710 NA. Casi un 30% de NA

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_pc_trast_parking, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_pc_trast_parking <- ifelse(is.na(df$IDEA_pc_trast_parking), "YES", "NO")
sum(df1$IDEA_pc_trast_parking == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_pc_trast_parking, y = TARGET, fill=IDEA_pc_trast_parking)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_ind_tienda df[,33]
############################

boxplot(df$IDEA_ind_tienda)

hist(df$IDEA_ind_tienda)

summary(df$IDEA_ind_tienda)
# Mediana 0.33  Media 0.3284
# 2717 NA. Casi un 30% de NA. 7 más que los anteriores

table(df$IDEA_ind_tienda)
#Lo gordo es cero (2071). El resto +- repartido

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_ind_tienda, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_ind_tienda <- ifelse(is.na(df$IDEA_ind_tienda), "YES", "NO")
sum(df1$IDEA_ind_tienda == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_ind_tienda, y = TARGET, fill=IDEA_ind_tienda)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_ind_turismo df[,34]
############################

boxplot(df$IDEA_ind_turismo)

hist(df$IDEA_ind_turismo)

summary(df$IDEA_ind_turismo)
# Mediana 0.23  Media 0.3246
# 2717 NA. Casi un 30% de NA

table(df$IDEA_ind_turismo)
# Picos en 0.00 (1672) y 1.00 (834)

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_ind_turismo, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_ind_turismo <- ifelse(is.na(df$IDEA_ind_turismo), "YES", "NO")
sum(df1$IDEA_ind_turismo == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_ind_turismo, y = TARGET, fill=IDEA_ind_turismo)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable



############################
# df$IDEA_ind_alimentacion df[,35]
############################

boxplot(df$IDEA_ind_alimentacion)

hist(df$IDEA_ind_alimentacion)

summary(df$IDEA_ind_alimentacion)
# Mediana 0.23  Media 0.259
# 2717 NA. Casi un 30% de NA

table(df$IDEA_ind_alimentacion)
#Pico en 0.00. Caída a partir de 0.5 en adelante

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_ind_alimentacion, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_ind_alimentacion <- ifelse(is.na(df$IDEA_ind_alimentacion), "YES", "NO")
sum(df1$IDEA_ind_alimentacion == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_ind_alimentacion, y = TARGET, fill=IDEA_ind_alimentacion)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_ind_riqueza df[,36]
############################

boxplot(df$IDEA_ind_riqueza)

hist(df$IDEA_ind_riqueza)

summary(df$IDEA_ind_riqueza)
# Mediana 0.14  Media 0.1644
# 2631 NA. Casi un 30% de NA

table(df$IDEA_ind_riqueza)
# lo mollar está entre 0.09 y 0.23

df1 %>%
  ggplot(aes(x = IDEA_ind_riqueza, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_ind_riqueza <- ifelse(is.na(df$IDEA_ind_riqueza), "YES", "NO")
sum(df1$IDEA_ind_riqueza == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_ind_riqueza, y = TARGET, fill=IDEA_ind_riqueza)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_rent_alquiler df[,37]
############################

boxplot(df$IDEA_rent_alquiler)

hist(df$IDEA_rent_alquiler)

summary(df$IDEA_rent_alquiler)
# Mediana 5.185  Media 5.387
# 3062 NA. Un 30% de NA

table(df$IDEA_rent_alquiler)
# 64 tienen una rentabilidad justo de 12. Es probable que no les dejase meter un número mayor

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_rent_alquiler, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_rent_alquiler <- ifelse(is.na(df$IDEA_rent_alquiler), "YES", "NO")
sum(df1$IDEA_rent_alquiler == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_rent_alquiler, y = TARGET, fill=IDEA_rent_alquiler)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_ind_elasticidad df[,38]
############################

boxplot(df$IDEA_ind_elasticidad)

hist(df$IDEA_ind_elasticidad)

summary(df$IDEA_ind_elasticidad)
# Mediana 3  Media 3.055
# 5106 NA. Casi un 50% de NA

table(df$IDEA_ind_elasticidad)

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_ind_elasticidad, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_ind_elasticidad <- ifelse(is.na(df$IDEA_ind_elasticidad), "YES", "NO")
sum(df1$IDEA_ind_elasticidad == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_ind_elasticidad, y = TARGET, fill=IDEA_ind_elasticidad)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_ind_liquidez df[,39]
############################

boxplot(df$IDEA_ind_liquidez)

hist(df$IDEA_ind_liquidez)

summary(df$IDEA_ind_liquidez)
# Mediana 0.00  Media 0.015
# 5106 NA. Casi un 50% de NA

table(df$IDEA_ind_liquidez)

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_ind_liquidez, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_ind_liquidez <- ifelse(is.na(df$IDEA_ind_liquidez), "YES", "NO")
sum(df1$IDEA_ind_liquidez == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_ind_liquidez, y = TARGET, fill=IDEA_ind_liquidez)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_unitprice_sale_residential df[,40]
############################

boxplot(df$IDEA_unitprice_sale_residential)

hist(df$IDEA_unitprice_sale_residential)

summary(df$IDEA_unitprice_sale_residential)
# Mediana 910.1  Media 1016.9
# 2635 NA

# 26,46% de NAs
sum(is.na(df$IDEA_unitprice_sale_residential))/nrow(df)

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_unitprice_sale_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_unitprice_sale_residential <- ifelse(is.na(df$IDEA_unitprice_sale_residential), "YES", "NO")
sum(df1$IDEA_unitprice_sale_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_unitprice_sale_residential, y = TARGET, fill=IDEA_unitprice_sale_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable



############################
# df$IDEA_price_sale_residential df[,41]
############################

boxplot(df$IDEA_price_sale_residential)

hist(df$IDEA_price_sale_residential)

summary(df$IDEA_price_sale_residential)
# Mediana 125046  Media 144891
# 2635 NA. Casi un 27% de NA

# Mirar cola

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_price_sale_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_price_sale_residential <- ifelse(is.na(df$IDEA_price_sale_residential), "YES", "NO")
sum(df1$IDEA_price_sale_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_price_sale_residential, y = TARGET, fill=IDEA_price_sale_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_stock_sale_residential df[,42]
############################

boxplot(df$IDEA_stock_sale_residential)

hist(df$IDEA_stock_sale_residential)

summary(df$IDEA_stock_sale_residential)
# Mediana 954  Media 1098
# 2635 NA. Casi un 27% de NA

# no hay relación con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_stock_sale_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_stock_sale_residential <- ifelse(is.na(df$IDEA_stock_sale_residential), "YES", "NO")
sum(df1$IDEA_stock_sale_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_stock_sale_residential, y = TARGET, fill=IDEA_stock_sale_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_demand_sale_residential df[,43]
############################

boxplot(df$IDEA_demand_sale_residential)

hist(df$IDEA_demand_sale_residential)

summary(df$IDEA_demand_sale_residential)
# Mediana 49.16  Media 48.77
# 2635 NA. Casi un 27% de NA

table(df$IDEA_demand_sale_residential)
# Mirar. 58 ceros, y a partir de ahí comienza en 44.72

# los ceros igual no nos dejan ver si hay relación
df1 %>%
  ggplot(aes(x = IDEA_demand_sale_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# no hay relación aún así con la variable TARGET
df1 %>% filter(df$IDEA_demand_sale_residential != 0) %>% 
  ggplot(aes(x = IDEA_demand_sale_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_demand_sale_residential <- ifelse(is.na(df$IDEA_demand_sale_residential), "YES", "NO")
sum(df1$IDEA_demand_sale_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_demand_sale_residential, y = TARGET, fill=IDEA_demand_sale_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_unitprice_rent_residential df[,44]
############################

boxplot(df$IDEA_unitprice_rent_residential)

hist(df$IDEA_unitprice_rent_residential)

summary(df$IDEA_unitprice_rent_residential)
# Mediana 4.810  Media 5.165
# 2998 NA

# 30,10% de NAs
sum(is.na(df$IDEA_unitprice_rent_residential))/nrow(df)

table(df$IDEA_unitprice_rent_residential)
# Quizás 1 sea un outlier, pero no es una locura y no estorba

# no hay relación aún así con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_unitprice_rent_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_unitprice_rent_residential <- ifelse(is.na(df$IDEA_unitprice_rent_residential), "YES", "NO")
sum(df1$IDEA_unitprice_rent_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_unitprice_rent_residential, y = TARGET, fill=IDEA_unitprice_rent_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_price_rent_residential df[,45]
############################

boxplot(df$IDEA_price_rent_residential)

hist(df$IDEA_price_rent_residential)

summary(df$IDEA_price_rent_residential)
# Mediana 524.1  Media 601.8
# 2988 NA. Casi un 30% de NA

# no hay relación aún así con la variable TARGET
df1 %>%
  ggplot(aes(x = IDEA_price_rent_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_price_rent_residential <- ifelse(is.na(df$IDEA_price_rent_residential), "YES", "NO")
sum(df1$IDEA_price_rent_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_price_rent_residential, y = TARGET, fill=IDEA_price_rent_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_stock_rent_residential df[,46]
############################

boxplot(df$IDEA_stock_rent_residential)

hist(df$IDEA_stock_rent_residential)

summary(df$IDEA_stock_rent_residential)
# Mediana 57  Media 116.1
# 2988 NA. Casi un 30% de NA

table(df$IDEA_stock_rent_residential)
# Mirar: hay seis de 5685

# Igual nos ocultan cosas el bloque de la derecha
df1 %>% 
  ggplot(aes(x = IDEA_stock_rent_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# no hay relación aún así con la variable TARGET
df1 %>% filter(IDEA_stock_rent_residential<900) %>% 
  ggplot(aes(x = IDEA_stock_rent_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_stock_rent_residential <- ifelse(is.na(df$IDEA_stock_rent_residential), "YES", "NO")
sum(df1$IDEA_stock_rent_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_stock_rent_residential, y = TARGET, fill=IDEA_stock_rent_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable


############################
# df$IDEA_demand_rent_residential df[,47]
############################

boxplot(df$IDEA_demand_rent_residential)

hist(df$IDEA_demand_rent_residential)

summary(df$IDEA_demand_rent_residential)
# Mediana 51.39  Media 47.53
# 2635 NA. Casi un 27% de NA

table(df$IDEA_demand_rent_residential)
# Mirar: Hay 563 ceros. El siguiente valor es 48.21

# El cero no nos deja ver el bosque
df1 %>%
  ggplot(aes(x = IDEA_demand_rent_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# no hay relación aún así con la variable TARGET
df1 %>% filter(IDEA_demand_rent_residential !=0) %>% 
  ggplot(aes(x = IDEA_demand_rent_residential, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# Probamos la misma variable con el TARGET diferenciando los que tienen y no tienen NAs
df1$IDEA_demand_rent_residential <- ifelse(is.na(df$IDEA_demand_rent_residential), "YES", "NO")
sum(df1$IDEA_demand_rent_residential == "YES") #coincide con los NAs, está bien hecho

df1 %>%
  ggplot(aes(x = IDEA_demand_rent_residential, y = TARGET, fill=IDEA_demand_rent_residential)) +
  geom_bar(stat="summary", fun.y="mean")
# Eliminamos la variable



############################
# df$GA_page_views df[,48]
############################

boxplot(df$GA_page_views)

hist(df$GA_page_views)

summary(df$GA_page_views)
# Mediana 30  Media 191
# 0 NA.

tail(table(df$GA_page_views))
# Mirar: hay un valor de 13698

df1 %>%
  ggplot(aes(x = log1p(GA_page_views), y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# le hacemos logaritmo a la variable
df1$GA_page_views <- log1p(df$GA_page_views)

# tiene una mejor distribución
hist(df1$GA_page_views)




############################
# df$GA_mean_bounce df[,49]
############################

boxplot(df$GA_mean_bounce)

hist(df$GA_mean_bounce)

summary(df$GA_mean_bounce)
# Mediana 14.29  Media 18.72
# 0 NA.

tail(table(df$GA_mean_bounce))
# Hay 123 entradas donde el usuario abandona sin interactuar con la web

df1 %>%
  ggplot(aes(x = GA_mean_bounce, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)


# le hacemos logaritmo a la variable
df1$GA_mean_bounce <- log1p(df$GA_mean_bounce)

# tiene una mejor distribución
hist(df1$GA_mean_bounce)




############################
# df$GA_exit_rate df[,50]
############################

boxplot(df$GA_exit_rate)

hist(df$GA_exit_rate)

summary(df$GA_exit_rate)
# Mediana 16.96  Media 22.21
# 0 NA.

head(table(df$GA_exit_rate))
# 210 usuarios cerraron el navegador sin visitar más páginas

df1 %>%
  ggplot(aes(x = GA_exit_rate, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# le hacemos logaritmo a la variable
df1$GA_exit_rate <- log1p(df$GA_exit_rate)

# tiene una mejor distribución
hist(df1$GA_exit_rate)



############################
# df$GA_quincena_ini df[,51]
############################

boxplot(df$GA_quincena_ini)

hist(df$GA_quincena_ini)

summary(df$GA_quincena_ini)
# Mediana 9.00  Media 14.42
# 0 NA

table(df$GA_quincena_ini)
# lo gordo está en 1 (2188)

df1 %>%
  ggplot(aes(x = GA_quincena_ini, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# le hacemos logaritmo a la variable. Ojo, como no hay ningún cero, podemos meter un log, no un log1p
df1$GA_quincena_ini <- log1p(df$GA_quincena_ini)

# tiene una mejor distribución
hist(df1$GA_quincena_ini)

############################
# df$GA_quincena_ult df[,52]
############################

boxplot(df$GA_quincena_ult)

hist(df$GA_quincena_ult)

summary(df$GA_quincena_ult)
# Mediana 46.00  Media 42.55
# 0 NA.

table(df$GA_quincena_ult)

df1 %>%
  ggplot(aes(x = GA_quincena_ult, y = TARGET)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "loess", se = F)

# le hacemos logaritmo a la variable. Ojo, como no hay ningún cero, podemos meter un log, no un log1p
df1$GA_quincena_ult <- log1p(df$GA_quincena_ult)

# tiene una PEOR distribución, mejor lo dejamos como estaba
hist(df1$GA_quincena_ult)

df1$GA_quincena_ult <- df$GA_quincena_ult




