## EXPLORATORIO VARIABLES
library(data.table)
library(dplyr)
library(ggplot2)
library(outliers)

df <-  fread('Modelar_UH2019.txt', encoding = 'Latin-1')

str(df)


##cambiamos el id a char.

df$HY_id <- as.character(df$HY_id)

#vamos a ver si el target varía por provincia

df$HY_provincia <- as.factor(df$HY_provincia)

df %>%
  
  group_by(HY_provincia) %>%
  
  summarise(media_target = mean(TARGET)) %>%
  
  ggplot(aes(x = HY_provincia, y = media_target)) +
  
  geom_point()


ggplot(df, aes(x = TARGET, fill = HY_provincia)) +
  geom_histogram()
## Se observan diferencias significativas en función de la provincia a la que pertenece la casa.

##Ahora vamos a probar lo mismo con el tipo de vivienda.


df %>%
  
  group_by(HY_tipo) %>%
  
  summarise(media_target = mean(TARGET)) %>%
  
  ggplot(aes(x = HY_tipo, y = media_target)) +
  
  geom_point()
##De nuevo, parece que hay diferencias en el tiempo de media que la gente pasa viendo
## la página en función del tipo de vivienda que sea. 

df %>%
  
  ggplot(aes(x = HY_metros_totales)) +
  
  geom_histogram(bins = 100)
##Aquí vemos que tenemos al menos 1 outlier...

boxplot(df$HY_metros_totales)
#Y vemos que no es 1, sino que hay varios; estas observaciones creo que habría que quitarlas. 

outlier(df$HY_metros_totales)

#df1 <-  df[df$HY_metros_totales != 1820000, ]



df1 %>%
  
  ggplot(aes(x = log1p(HY_metros_totales))) +
  
  geom_histogram(bins = 100)

#Vale, esto está mucho mejor. Decidimos entonces sacar el logaritmo de esta varable para que nos salgan cosas razonables.

df1$HY_metros_totales <- log1p(df1$HY_metros_totales)


### METROS ÚTILES ###

df1 %>%
  
  ggplot(aes(x = log1p(HY_metros_utiles))) +
  
  geom_histogram(bins = 100)

#como hay muchísimos valores ausentes para esta variable, vamos a meterle en los NA el valor de los metros totales

df1[is.na(df1$HY_metros_utiles), 'HY_metros_utiles'] <- df1$HY_metros_totales[is.na(df1$HY_metros_utiles)]

df1$HY_metros_utiles <- log1p(df1$HY_metros_utiles)

### Distribución ###

#df1$HY_distribucion <- as.factor(df1$HY_distribucion)

#levels(df1$HY_distribucion)


### Número de baños ###

df1 %>%
  
  ggplot(aes(x = HY_num_banos)) +
  
  geom_histogram(bins = 100)

boxplot(df1$HY_num_banos)
#Hay una casa en la que pone que hay 100 baños... No me cuadra. 

df1[df1$HY_num_banos == max(df1$HY_num_banos), c("HY_precio", "TARGET")]

#una forma de solucionar este problema con el número de baños sería sacar el logaritmo

hist(log1p(df1$HY_num_banos))

df1$HY_num_banos <- log1p(df1$HY_num_banos)


### RELACIÓN PRECIO-TARGET ####

df1 %>%
  
  ggplot(aes(x = log1p(HY_precio), y = TARGET)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = F)

#No hay demasiada relación entre precio y tiempo de visita...

df1 %>%
  
  ggplot(aes(x = log1p(HY_precio) / HY_metros_totales, y = log1p(TARGET))) +
  
  geom_point() +
  
  geom_smooth(method = "lm", se = F)

