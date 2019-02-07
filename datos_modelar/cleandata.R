
library(data.table)
library(dplyr)
library(mltools)

d_or <- fread('../datos_originales/Modelar_UH2019.txt', encoding = 'UTF-8')

imagenes <- fread('../datos_originales/numero_imagenes_train.csv')

d_or <- left_join(d_or, imagenes, by = 'HY_id')

require(ggplot2)
d_or %>%
  
  group_by(conteo) %>%
  
  summarise(target = mean(TARGET), num = n()) %>%
  
  
  ggplot(aes(x = conteo, y = target)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip()



d_or$conteo[is.na()]


limpiar <- function(df = d_or) {
  
  categoricas <- df[, c("HY_provincia", "HY_tipo")]
  
  df[, c("HY_provincia", "HY_tipo")] <- NULL
  
  categoricas_dummy <- one_hot(as.data.table(categoricas))
  
  categoricas_dummy <- data.frame(categoricas_dummy)
  
  rellenar_metros <- df %>%
    
    group_by(HY_provincia, HY_tipo) %>%
    
    summarise(metros = median(HY_metros_totales, na.rm=T)) 
  
  for(i in 1:nrow(df)) {
    
    if(is.na(df$HY_metros_totales[i])) {
      
      value <- rellenar_metros[which(rellenar_metros$HY_provincia == df[i, "HY_provincia"] & rellenar_metros$HY_tipo == df[i, "HY_tipo"]), "metros"]
      
      df[i, "HY_metros_totales"] <- value
      
    }
    
  }
  
  
  
  
  
  
  
}