## EXPLORATORIO VARIABLES
library(data.table)
library(dplyr)
library(ggplot2)
library(outliers)

df <-  fread('Modelar_UH2019.txt', encoding = 'UTF-8')

provincias <- fread('provincias.csv', encoding = "UTF-8")

names(df)[3] <- "provincia"

df <- left_join(df, provincias, by = "provincia")

names(df)[3] <- "HY_provincia"

str(df)

names(df)[1:25]
##cambiamos el id a char.

df$HY_id <- as.character(df$HY_id)

#vamos a ver si el target varía por provincia

df$HY_provincia <- as.factor(df$HY_provincia)

df %>%
  
  group_by(HY_provincia) %>%
  
  summarise(media_target = mean(TARGET)) %>%
  
  ggplot(aes(x = reorder(HY_provincia, media_target), y = media_target)) +
  geom_bar(stat = 'identity')+
  coord_flip()

df %>%
  
  group_by(ccaa) %>%
  
  summarise(media_target = mean(TARGET)) %>%
  
  ggplot(aes(x = reorder(ccaa, media_target), y = media_target)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip()


ggplot(df, aes(x = TARGET, fill = HY_provincia)) +
  geom_histogram()
## Se observan diferencias significativas en función de la provincia a la que pertenece la casa.

##Ahora vamos a probar lo mismo con el tipo de vivienda.


df %>%
  
  group_by(HY_tipo) %>%
  
  summarise(media_target = mean(TARGET)) %>%
  
  ggplot(aes(x = reorder(HY_tipo, media_target), y = media_target)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip()

##De nuevo, parece que hay diferencias en el tiempo de media que la gente pasa viendo
## la página en función del tipo de vivienda que sea. 

sum(is.na(df$HY_tipo)) #sin ningún valor ausente!.

df %>%
  
  ggplot(aes(x = HY_metros_totales)) +
  
  geom_histogram(bins = 100)
##Aquí vemos que tenemos al menos 1 outlier...

boxplot(df$HY_metros_totales)
#Y vemos que no es 1, sino que hay varios; estas observaciones creo que habría que quitarlas. 

outlier(df$HY_metros_totales)



#df1 <-  df[df$HY_metros_totales != 1820000, ]
df1 <- df


df1 %>%
  
  ggplot(aes(x = log1p(HY_metros_totales))) +
  
  geom_histogram(bins = 100)

#Vale, esto está mucho mejor. Decidimos entonces sacar el logaritmo de esta varable para que nos salgan cosas razonables.

df1 %>% 
  
  ggplot(aes(x = log1p(HY_metros_totales), y = log1p(TARGET), color = HY_tipo)) +
  
  geom_point() +
  
  geom_smooth(method="loess", se = T)

sum(is.na(df1$HY_metros_totales))

df1 %>%
  
  ggplot(aes(x = log1p(HY_metros_totales), y = TARGET)) +
  
  geom_point()



#Además, hay que tomar decisiones con los 0's, entre otras cosas, además de con los valores ausentes. 

metros_agrupados <- df1 %>%
  
  group_by(HY_provincia, HY_tipo) %>%
  
  summarise(mediana = median(HY_metros_totales, na.rm = T))

fill_metros <- function(df = df1, grouped = metros_agrupados) {
  
  new <- df
  
  for(i in 1:nrow(new)) {
    
    if(is.na(new[i, 'HY_metros_totales'])) {
      
      if(!is.na(grouped[which(grouped$HY_provincia == new$HY_provincia[i] & grouped$HY_tipo == new$HY_tipo[i]), 'mediana'])){
        
        valor <- grouped[which(grouped$HY_provincia == new$HY_provincia[i] & grouped$HY_tipo == new$HY_tipo[i]), 'mediana']
        
        new[i, 'HY_metros_totales'] <- valor
        
      } else {
        
        newgr <- data.frame(new %>%
          
          group_by(HY_tipo) %>%
          
          summarise(mediana = median(HY_metros_totales, na.rm=T)) )
        
        valor <- newgr[which(newgr$HY_tipo == new$HY_tipo[i]), 'mediana']
        
        new[i, 'HY_metros_totales'] <- valor
        
      }
      
    } else {
      
      next
    }
    
  }
  
  return(new)
}

df1 <- fill_metros()

sum(is.na(df1$HY_metros_totales))



df1$HY_metros_totales <- log1p(df1$HY_metros_totales)


### METROS ÚTILES ###

df1 %>%
  
  ggplot(aes(x = log1p(HY_metros_utiles))) +
  
  geom_histogram(bins = 100)


df1 %>%
  
  ggplot(aes(x = log1p(HY_metros_utiles), y = HY_metros_totales)) +
  
  geom_point()

#como hay muchísimos valores ausentes para esta variable, vamos a meterle en los NA el valor de los metros totales

#df1[is.na(df1$HY_metros_utiles), 'HY_metros_utiles'] <- df1$HY_metros_totales[is.na(df1$HY_metros_utiles)]


df1$HY_metros_utiles <- NA

#df1$HY_metros_utiles <- log1p(df1$HY_metros_utiles)

### Distribución ###

#df1$HY_distribucion <- as.factor(df1$HY_distribucion)

#levels(df1$HY_distribucion)


### Número de baños ###

df1 %>%
  
  ggplot(aes(x = HY_num_banos)) +
  
  geom_histogram(bins = 100)

boxplot(df1$HY_num_banos)
#Hay una casa en la que pone que hay 100 baños... No me cuadra. 

df1[df1$HY_num_banos == max(df1$HY_num_banos), c("HY_num_banos", "HY_precio", "TARGET", "HY_tipo", "IDEA_area")]

#viendo esto, me parece evidente que no todos estos edificios pueden tener 99 baños; especialmente no los pisos.

df1 %>%
  
  group_by(HY_tipo) %>%
  
  summarise(banos = median(HY_num_banos))


df1[df1$HY_num_banos == 99 & df1$HY_tipo == "Piso", "HY_num_banos"] <- 1


summary(df1$HY_num_banos)

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

##### PRECIO ANTERIOR ####

sum(is.na(df1$HY_precio_anterior)) #tiene muchísimos valores ausentes...

df1$bajado_precio <- ifelse(df1$HY_precio < df1$HY_precio_anterior, 1, 0)

df1$bajado_precio[is.na(df1$bajado_precio)] <- 0

df1 %>%
  
  group_by(bajado_precio) %>%
  
  ggplot(aes(x = bajado_precio, y = TARGET)) +
  
  geom_point() +
  
  geom_jitter()

df1 %>%
  
  group_by(bajado_precio) %>%
  
  summarise(target = mean(TARGET)) %>%
  
  ggplot(aes(x = bajado_precio, y = target)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip()

#parece apenas haber diferencias... no merece la pena incluir esta variable. 


###HY_cert_energ###

summary(as.factor(df1$HY_cert_energ))

df1 %>%
  
  group_by(HY_cert_energ) %>%
  
  summarise(media_target = mean(TARGET)) %>%
  
  ggplot(aes(x = HY_cert_energ, y = media_target)) +
  
  geom_point()


#Parece que si existe una diferencia en la media del target en función de la certificación energética

# utilizando este link https://ovacen.com/certificado-energetico/ 
#yo diría de pasar la certificación energética "" a B, porque todos los inmuebles están obligados a tener una. 

df1$HY_cert_energ[df1$HY_cert_energ == ""] <- "B"

df1$HY_cert_energ <- as.factor(df1$HY_cert_energ)



### HY_num_terrazas ###

boxplot(df1$HY_num_terrazas)

hist(df1$HY_num_terrazas)

#hist(log1p(df1$HY_num_terrazas))

cor(df1$HY_num_terrazas, df1$TARGET)

plot(df1$HY_num_terrazas, df1$TARGET)
#tiene pinta de que esta variable va a ser poco importante...

df1$tiene_terraza <- ifelse(df1$HY_num_terrazas > 0, 1, 0)

df1 %>%
  
  group_by(tiene_terraza) %>%
  
  summarise(target = mean(TARGET)) %>%
  
  ggplot(aes(x = tiene_terraza, y = target)) +
  
  geom_point()

#vale, ahí si vemos una relación clara; las casas con terraza se ven más. 


df1 %>%
  
  group_by(tiene_terraza) %>%
  
  ggplot(aes(x = tiene_terraza, y = TARGET)) +
  
  geom_point() #sin embargo cuando vemos todas las observaciones no queda tan claro...


df1 %>%
  
  group_by(tiene_terraza) %>%
  
  summarise(target = median(TARGET)) %>%
  
  ggplot(aes(x = tiene_terraza, y = target)) +
  
  geom_point()

### HY_ascensor###

summary(df1$HY_ascensor)

df1$HY_ascensor[1:10]

#esto ya es una variable binaria

sum(is.na(df1$HY_ascensor))


df1 %>%
  
  group_by(HY_ascensor) %>%
  
  filter(HY_tipo == "Piso") %>%
  
  summarise(target = median(TARGET)) %>%
  
  ggplot(aes(x = HY_ascensor, y = target)) +
  
  geom_point()


#aquí las diferencias si nos fijamos bien son muy pequeñas entre las casas con y sin ascensor. 
#ni siquiera son significativas cuando le metemos el filtro de piso...

####Trastero####

summary(df1$HY_trastero) #otra variable binaria.
#solo un 6% de las casas tienen trastero.

df1%>%
  
  group_by(HY_trastero) %>%
  
  summarise(target = mean(TARGET)) %>%
  
  ggplot(aes(x = HY_trastero, y = target)) +
  
  geom_point()

#alguna diferencia aunque tampoco demasiado significativas; la gente mira algo más las casas con trastero.

sum(is.na(df1$HY_trastero))

df1 %>%
  group_by(HY_trastero) %>%
  
  ggplot(aes(x = HY_trastero, y = TARGET)) +
  
  geom_point() +
  geom_jitter()


### HY_num_garajes ####

summary(df1$HY_num_garajes)

#quizás sea buena idea agruparla en 0,1 ...

df1 %>%
  
  group_by(HY_num_garajes) %>%
  
  summarise(target = mean(TARGET)) %>%
  
  ggplot(aes(x = HY_num_garajes, y = target)) +
  
  geom_point()

#está claro, cuantos más garajes tenemos, nos encontramos con que la media del target es mayor. 
#las diferencias son mayores entre tener 0 o 1 garajes que entre 1 y 2... 
#cuando veáis esto, decid si os parece mejor pasarlo a 0,1 o dejarlo como está. 


df1 %>%
  
  group_by(HY_num_garajes) %>%
  
  ggplot(aes(x = HY_num_garajes, y = TARGET)) +
  
  geom_point() + geom_jitter()

df1$tiene_garaje <- ifelse(df1$HY_num_garajes > 0, 1, 0)

df1 %>%
  
  group_by(tiene_garaje) %>%
  
  summarise(target = median(TARGET)) %>%
  
  ggplot(aes(x = tiene_garaje, y = target)) +
  
  geom_point()


#aquí vemos unas diferencias más significativas...

df1 %>%
  
  group_by(tiene_garaje) %>%
  
  ggplot(aes(x = tiene_garaje, y = TARGET)) +
  
  geom_point()


summary(df1$tiene_garaje)
#solo el 9,6% de las casas tienen garaje, por lo que puede ser una buena idea dejarlo en variable binaria.



#### HY_precio ####

boxplot(df1$HY_precio) #muy asimétrica...

hist(df1$HY_precio)

hist(log1p(df1$HY_precio)) #esto está bastante mejor... mucho mejor sacando el logaritmo. 

df1 %>%
  
  ggplot(aes(x = log1p(HY_precio), y = log1p(TARGET))) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = T)
#apenas se nota un efecto...

df1 %>%
  
  ggplot(aes(x = log1p(HY_precio), y = log1p(HY_precio_anterior))) +
  
  geom_point()

#parece casi una línea recta; puede ser una buena idea sustituir los ausentes en precio anterior por el precio actual...

hist(log1p(df1$HY_precio_anterior))

#se parece mucho al histograma del precio... 


#### IDEA_area ####

summary(df1$IDEA_area)

df1 %>%
  
  group_by(HY_tipo, HY_provincia) %>%
  
  summarise(area = median(IDEA_area, na.rm=T), num = n()) %>%
  
  ggplot(aes(x = HY_provincia, y = area, color = HY_tipo, size = num)) +
  
  geom_point()


#al haber diferencias significativas por provincia, una buena idea para imputar NAs puede ser imputarlos por provincia
#tambien hay diferencias significativas por tipo de casa...; vamos a usar esto también

df1 %>%
  
  ggplot(aes(x = log1p(IDEA_area), y = TARGET)) +
  
  geom_point()

sum(is.na(df1$IDEA_area))


area_por_provincia <- data.frame(df1 %>%
  
  group_by(HY_tipo, HY_provincia) %>%
  
  summarise(area = mean(IDEA_area, na.rm = T)))

fill_area_nas <- function(df = df1, medias = area_por_provincia) {
  
  for(i in 1:nrow(df)) {
    
    if(is.na(df[i, "IDEA_area"])) {
      
      media <- medias[which(medias$HY_provincia == df$HY_provincia[i] & medias$HY_tipo == df$HY_tipo[i]), "area"]
      
      df[i, "IDEA_area"] <- media
      
    } else {
      
      next
    }
    
  }
  
  return(df)
}

df2 <- fill_area_nas()

sum(is.na(df2$IDEA_area))

#Vale, ahora ya lo tenemos casi limpio de NAs.

hist(log1p(df1$IDEA_area))
hist(log1p(df2$IDEA_area))


####POblación####

df1 %>%
  
  ggplot(aes(x = IDEA_poblacion^2, y = log1p(TARGET))) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = T)

#nada, parece que la población no afecta al target...
#igual no es necesario meter poblacion...


#Densidad #####
df1 %>%
  
  ggplot(aes(x = IDEA_densidad^2, y = TARGET)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = F)
#tampoco parece ser relevante la densidad...


####IDEA_pc_1960####

boxplot(df1$IDEA_pc_1960)

hist(df1$IDEA_pc_1960)

hist(log1p(df1$IDEA_pc_1960)) #lo normaliza un poco...

#pero tampoco mucho.


df1 %>%
  
  ggplot(aes(x = log1p(IDEA_pc_1960), y = TARGET)) +
  
  geom_point()


df1 %>%
  
  ggplot(aes(x = IDEA_pc_1960_69, y = TARGET)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = T)

df1 %>%
  
  ggplot(aes(x = IDEA_pc_1970_79, y = TARGET)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = F)


df1 %>%
  
  ggplot(aes(x = IDEA_pc_1980_89, y = TARGET)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = F)


df1 %>%
  
  ggplot(aes(x = IDEA_pc_1990_99, y = TARGET)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = F)

df1 %>%
  
  ggplot(aes(x = IDEA_pc_2000_10, y = TARGET)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", se = F)

#en todas se repiten los mismos 2710 valores ausentes...

casas_nuevas <- c()

for(i in 1:nrow(df1)) {
  
  if(is.na(df1$IDEA_pc_1960[i])){
    
    casas_nuevas <- c(casas_nuevas, 1)
  } else {
  
  
    if(df1$IDEA_pc_1960[i] <= 0.10 & df1$IDEA_pc_1960_69[i] <= 0.20 
      & df1$IDEA_pc_1970_79[i] <= 0.20 & df1$IDEA_pc_1980_89[i] <= 0.25
      & df1$IDEA_pc_1990_99[i] <= 0.30 & df1$IDEA_pc_2000_10[i] <= 0.75) {
    
      casas_nuevas <- c(casas_nuevas, 1)
    
  } else {
    
    casas_nuevas <- c(casas_nuevas, 0)
    
    
  }
  
  }
}

df1$nueva <- casas_nuevas

df1 %>%
  
  group_by(nueva) %>%
  
  ggplot(aes(x = nueva, y = TARGET)) +
  
  geom_point()

#la alternativa es tratar los NAs como si se trataran de casas antiguas (0). 

casas_nuevas2 <- c()


for(i in 1:nrow(df1)) {
  
  if(is.na(df1$IDEA_pc_1960[i])){
    
    casas_nuevas2 <- c(casas_nuevas2, 0)
    
  } else {
    
    
    if(df1$IDEA_pc_1960[i] <= 0.10 & df1$IDEA_pc_1960_69[i] <= 0.20 
       & df1$IDEA_pc_1970_79[i] <= 0.20 & df1$IDEA_pc_1980_89[i] <= 0.25
       & df1$IDEA_pc_1990_99[i] <= 0.30 & df1$IDEA_pc_2000_10[i] <= 0.75) {
      
      casas_nuevas2 <- c(casas_nuevas2, 1)
      
    } else {
      
      casas_nuevas2 <- c(casas_nuevas2, 0)
      
      
    }
    
  }
}


df1$nuevas2 <- casas_nuevas2

df1 %>%
  
  group_by(nuevas2) %>%
  
  ggplot(aes(x = nuevas2, y = TARGET)) +
  
  geom_point() + geom_jitter()

#parece que el grupo en el que pongamos los NAs es determinante; también podríamos agrupar por esto...

df1$IDEA_pc_isna <- ifelse(is.na(df1$IDEA_pc_1960), 1, 0)

df1 %>%
  
  group_by(IDEA_pc_isna) %>%
  
  ggplot(aes(x = IDEA_pc_isna, y = TARGET)) +
  
  geom_point()

df1 %>%
  
  group_by(nueva) %>%
  
  summarise(target = mean(TARGET))
#apenas hay diferencias en este caso...

df1 %>%
  
  group_by(nuevas2) %>%
  
  summarise(target = mean(TARGET))
#algo más de diferencia...

df1 %>%
  
  group_by(IDEA_pc_isna) %>%
  
  summarise(target = mean(TARGET))
#algo menos de diferencia... igual lo mejor sería quedarse con la segunda opción. 

#####ANTIGUEDAD#####

summary(df1$HY_antiguedad)

df1 %>%
  
  ggplot(aes(x = HY_antiguedad, y = TARGET)) +
  
  geom_point()


df1$antiguedad_conocida <- ifelse(!is.na(df1$HY_antiguedad), 1, 0)

df1 %>%
  
  group_by(antiguedad_conocida, HY_tipo) %>%
  
  ggplot(aes(x = antiguedad_conocida, y = TARGET, color = HY_tipo)) +
  
  geom_point() + geom_jitter()

df1 %>%
  
  group_by(antiguedad_conocida) %>%
  
  summarise(target = mean(TARGET))
#no hay absolutamente ninguna diferencia entre si se conoce la antiguedad de la casa o no...


# SACAR DATOS FINALES PARA MODELIZAR Y REALIZACIÓN DE CLUSTERING----

X <- df1

X$HY_id <- NULL

X$HY_descripcion <- NULL

X$HY_distribucion <- NULL

X$HY_tipo <- as.factor(X$HY_tipo)

X_no_nas <- X[complete.cases(X), ]

dummy <- X_no_nas[ , c("HY_provincia", "HY_tipo", "HY_cert_energ")]

X_no_nas[ , c("HY_provincia", "HY_tipo", "HY_cert_energ")] <- NULL

library(mltools)

dummy_oh <- one_hot(as.data.table(dummy))

du <- data.frame(dummy_oh)

X_no_nas <- scale(X_no_nas)

X_c <- cbind(X_no_nas, du)

X_c$TARGET <- X_c$TARGET*10

library(flexclust)
library(factoextra)
library(cluster)

kmeans_fit <-  kmeans(X_c, centers = 2, iter.max = 50, nstart = 100)

kmeans_fit$centers

fviz_cluster(kmeans_fit, data = X_c[, 1:47], geom = c("point"),ellipse.type = 'norm', pointsize=1)+
  theme_minimal()+geom_text(label=as.character((X_c$TARGET/10)*sd(df1$TARGET) + mean(df1$TARGET)),hjust=0, vjust=0,size=2,check_overlap = T)+scale_fill_brewer(palette="Paired")


groups = kmeans_fit$cluster


d <- dist(X_c, method="euclidean")  
sil = silhouette(groups, d)
plot(sil, col=1:4, main="", border=NA)
summary(sil)

X_c$groups <- groups

X_c$TARGET_real <- (X_c$TARGET/10)*sd(df1$TARGET) + mean(df1$TARGET)

X_c %>%
  
  group_by(groups) %>%
  
  summarise(target = mean(TARGET_real, na.rm = T),
            numero = n()) %>%
  
  ggplot(aes(x = groups, y = target, color = groups, size = numero)) +
  
  geom_point()

#Parece que con el análisis de cluster que hemos hecho sí que podemos identificar diferentes tiempos de estancia en las páginas web. 
#Una buena idea sería hacer un modelo para cada cluster. Es una pena no tener más datos completos para tirar el modelo
#de aprendizaje no supervisado; ya que esto nos permitiría ver más tipos de casas y sacar más conclusiones. Creo que el siguiente
#paso va a ser eliminar algunas columnas ruidosas y quedarnos solamente con las que nos renten, para así hacer un modelo de clustering
#con más observaciones, y una vez hayamos conseguido un modelo que más o menos nos guste, podemos tirarle. 

gr <- X_c$groups

gr <- one_hot(as.data.table(gr))

X_c$groups <- NULL

X_c <- cbind(X_c, gr)

X_c$TARGET <- NULL

X_c$TARGET <- X[complete.cases(X), ]$TARGET

X_c$TARGET_real <- NULL

write.csv(X_c, "../datos_modelar/transformados01.csv", fileEncoding = "utf-8")


library(GGally)

ggcorr(X_c, label = F)
#la variable target está muy poco correlacionada con las demás...

require(mclust)

mcl <- Mclust(X_c)


X_c$gr2 <- mcl$classification

X_c %>%
  
  group_by(gr2) %>%
  
  summarise(target = mean(TARGET), num = n()) %>%
  
  ggplot(aes(x = gr2, y = target, size = num) ) +
  
  geom_point()

X_c$gr <- NULL

names(X_c)[ncol(X_c)] <- c("groups")


X_c %>%
  
  group_by(groups) %>%
  
  summarise(target = mean(TARGET), num = n()) %>%
  
  ggplot(aes(x = groups, y = target, size = num) ) +
  
  geom_point()

write.csv(X_c, "../datos_modelar/transformados01.csv", fileEncoding = "utf-8")


