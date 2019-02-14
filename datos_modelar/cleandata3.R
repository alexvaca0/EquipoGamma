
library(data.table)
library(plyr)
library(dplyr)
library(mltools)


d_or <- fread('../datos_originales/Modelar_UH2019.txt', encoding = 'UTF-8')

imagenes <- fread('../datos_originales/numero_imagenes_train.csv')

d_or <- left_join(d_or, imagenes, by = 'HY_id')

d_or[is.na(d_or$img_count), "img_count"] <- 0

require(ggplot2)

d_or %>%
  
  group_by(img_count) %>%
  
  summarise(target = mean(TARGET), num = n()) %>%
  
  
  ggplot(aes(x = img_count, y = target)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip()



#d_or$conteo[is.na()]

minmaxscaler = function(x){
  
  new = ((x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T)))
  
  return(new)
  
}


limpiar <- function(df = d_or) {
  
  #en el vector transformaciones sacamos solo las de tipo "log" etc...
  #además, también metemos las nuevas variables que incluyamos.
  
  
  
  transformaciones <- c()

  
  rellenar_metros <- df %>%
    
    group_by(HY_provincia, HY_tipo) %>%
    
    summarise(metros = median(HY_metros_totales, na.rm=T)) 
  
  for(i in 1:nrow(df)) {
    
    if(is.na(df$HY_metros_totales[i])) {
      
      value <- rellenar_metros[which(rellenar_metros$HY_provincia == df[i, "HY_provincia"] & rellenar_metros$HY_tipo == df[i, "HY_tipo"]), "metros"]
      
      df[i, "HY_metros_totales"] <- value
      
    }
    
  }
  
  df$HY_metros_utiles <- NULL
  
  banos_gr <- df %>%
    
    group_by(HY_tipo) %>%
    
    summarise(banos = median(HY_num_banos, na.rm=T))
  
  #df$HY_num_banos <- as.vector(df$HY_num_banos)
  
  for (i in 1:nrow(df)) {
    
    if(df$HY_num_banos[i] == 99) {
      
      value <- banos_gr[which(banos_gr$HY_tipo == df$HY_tipo[i]), "banos"]
      
      df[i, "HY_num_banos"] <- value
    }
  }
  
  df$HY_num_banos <- log1p(df$HY_num_banos)
  
  transformaciones <- c(transformaciones, "HY_num_banos a log1p")
  
  df$HY_precio <- log1p(df$HY_precio)
  
  transformaciones <- c(transformaciones, "precio a log1p")
  
  df$precio_metros <- df$HY_precio / df$HY_metros_totales
  
  df$HY_cert_energ[df$HY_cert_energ == ""] <- "B"
  
  #categoricas <- cbind(categoricas, df$HY_cert_energ)
  
  
  
  df$HY_num_terrazas <- ifelse(df$HY_num_terrazas > 0, 1, 0)
  
  df$HY_num_garajes <- ifelse(df$HY_num_garajes > 0, 1, 0)
  
  df$IDEA_area <- NULL
  
  df$IDEA_poblacion <- NULL
  
  df$IDEA_densidad <- NULL
  
  df[, c("IDEA_pc_1960", "IDEA_pc_1960_69", "IDEA_pc_1970_79", 
         "IDEA_pc_1980_89", "IDEA_pc_1990_99", "IDEA_pc_2000_10")] <- NULL
  
  df$IDEA_pc_comercio <- NULL
  
  df$HY_antiguedad <- ifelse(is.na(df$HY_antiguedad), 0, 1)
  
  df$IDEA_pc_industria <- ifelse(is.na(df$IDEA_pc_industria), 0, 1)
  
  df$IDEA_pc_oficina <- ifelse(is.na(df$IDEA_pc_oficina), 0, 1)
  
  df$IDEA_pc_otros <- ifelse(is.na(df$IDEA_pc_otros), 0, 1)
  
  df$IDEA_pc_residencial <- NULL
  
  df$IDEA_pc_trast_parking <- NULL
  
  df$IDEA_ind_tienda <- ifelse(is.na(df$IDEA_ind_tienda), 0, 1)
  
  df$IDEA_ind_turismo <- NULL
  
  df$IDEA_ind_liquidez <- ifelse(is.na(df$IDEA_ind_liquidez), 0, 1)
  
  df$IDEA_ind_alimentacion <- NULL
  
  df$IDEA_ind_riqueza <- ifelse(is.na(df$IDEA_ind_riqueza), 0, 1)
  
  df$IDEA_rent_alquiler <- ifelse(is.na(df$IDEA_rent_alquiler), 0, 1)
  
  df$IDEA_ind_elasticidad <- NULL
  
  for(col in c("IDEA_unitprice_sale_residential", "IDEA_price_sale_residential",
               "IDEA_stock_sale_residential", "IDEA_demand_sale_residential",
                "IDEA_unitprice_rent_residential", "IDEA_price_rent_residential",
               "IDEA_stock_rent_residential", "IDEA_demand_rent_residential")){
    
    df[, col] <- ifelse(is.na(df[, col]), 0, 1)
    
    
  }
   
  df$TARGET <- log1p(df$TARGET)
  
  transformaciones <- c(transformaciones, "target a log1p")
  
  #categoricas <- df$HY_provincia
  
  #categoricas <- cbind(categoricas, df$HY_tipo)
  
  #categoricas <- cbind(categoricas, df$HY_cert_energ)
  
  #categoricas <- data.frame(categoricas)
  
  categoricas <- df[, c("HY_provincia", "HY_tipo", "HY_cert_energ")]
  
  categoricas$HY_cert_energ <- as.factor(categoricas$HY_cert_energ)
  
  categoricas$HY_provincia <- as.factor(categoricas$HY_provincia)
  
  categoricas$HY_tipo <- as.factor(categoricas$HY_tipo)
  
  categoricas_dummy <- one_hot(as.data.table(categoricas))
  
  categoricas_dummy <- data.frame(categoricas_dummy)
  
  data_imagenes=fread('../datos_originales/data_imagenes_por_casa.csv')
  data_imagenes=data_imagenes[data_imagenes$dataset=="Train",-34]
  
  #df=join(df, data_imagenes, by="HY_id", type="full", match="first")
  
  df = merge(df, data_imagenes, by = "HY_id", all.x = T)
  
  df[is.na(df)] <- 0
  
  #d[is.na(df)] <- 0
  
  df$HY_id <- NULL
  
  df$HY_cod_postal <- NULL
  
  df$HY_descripcion <- NULL
  
  df$HY_distribucion <- NULL
  
  df$HY_provincia <- NULL
  
  df$HY_tipo <- NULL
  
  df$HY_cert_energ <- NULL
  
  df$HY_precio_anterior <- NULL
  
  
  #df[, c("HY_provincia", "HY_tipo", "HY_cert_energ")] <- NULL
  
  TARGET <- df[, "TARGET"]
  
  df$TARGET <- NULL
  
  df <- apply(df, 2, minmaxscaler)
  
  df <- cbind(df, TARGET)
  
  df <- data.frame(cbind(df, categoricas_dummy))
  
  df <- df[complete.cases(df), ]
  
  write.csv(df, 'transformados05.csv', row.names =F, col.names = T,
            fileEncoding = 'utf-8')
  
  write.csv(transformaciones, 'transformaciones_realizadas.csv', 
            row.names=F, col.names=F)
  
  return(df)
  
}

new_df <- limpiar()










