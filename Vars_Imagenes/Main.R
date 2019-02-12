#### Analisis de Imagenes Cajamar ####

# Paquetes #####
# For image processing
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")
# devtools::install_github("flovv/RoogleVision")
library(pacman)
p_load("dplyr")
p_load("data.table")
p_load("tidyr")
p_load("magick")
p_load("jpeg")
p_load("raster")
p_load("RoogleVision")
p_load("EBImage")
p_load("jsonlite")
p_load("RCurl")
p_load("googleAuthR")
p_load("ggplot2")
p_load("Rcpp")

MyGoogleApiKey=readChar('CloudVisionCredentials.txt', file.info('CloudVisionCredentials.txt')$size)

# Lectura de datos ####
imagenes=list.files(path = 'imagenes_inmuebles_haya/')
modelar=fread(file = 'Datasets_Reto_Modelling_UH2019/Modelar_UH2019.txt')
estimar=fread(file = 'Datasets_Reto_Modelling_UH2019/Estimar_UH2019.txt')

# Vars por casa ####
bdCasas = bdImg %>%
  group_by(HY_id) %>%
  summarise(conteo=n()) %>%
  mutate(dataset = 
           ifelse(HY_id %in% modelar$HY_id,'Train',
                ifelse(HY_id %in% estimar$HY_id, 'Test', 'None')))

# _CSVs con num img ----
# num_imagenes_total=bdImg %>%
#   group_by(HY_id) %>%
#   summarise(conteo=n())
# fwrite(num_imagenes_total, file = 'numero_imagenes_total.csv')
# 
# num_imagenes_train=x %>%
#   filter(HY_id %in% modelar$HY_id) %>%
#   group_by(HY_id) %>%
#   summarise(img_count=n())
# fwrite(num_imagenes_train, file = 'numero_imagenes_train.csv')
# num_imagenes_test=x %>%
#   filter(!(HY_id %in% modelar$HY_id)) %>%
#   group_by(HY_id) %>%
#   summarise(img_count=n())
# fwrite(num_imagenes_test, file = 'numero_imagenes_test.csv')


# Vars por imagen ####
bdImg=data.frame('filename'=imagenes) %>% #Nota: image_id NO es unico, solo por casa.
  separate(col = 'filename', sep = '__',into = c('HY_id','posifoto', 'image_id'), remove = F)

bdImg=bdImg %>% left_join(bdCasas[,c('HY_id', 'dataset')])


# Google Api ####

# creds = fromJSON('credentials.json')
# 
options("googleAuthR.client_id" = creds$web$client_id)
options("googleAuthR.client_secret" = creds$web$client_secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

# A manita ----
imageFile <- paste0('imagenes_inmuebles_haya/',imagenes[1])
txt <- base64Encode(readBin(imageFile, "raw", file.info(imageFile)[1, "size"]), "txt")
### create Request, following the API Docs.
body= paste0('{  "requests": [    {   "image": { "content": "',txt,'" }, "features": [  { "type": "LABEL_DETECTION", "maxResults": 20} ],  }    ],}')
## generate function call
simpleCall <- gar_api_generator(baseURI = "https://vision.googleapis.com/v1/images:annotate", http_header="POST" )
## set the request!
pp <- simpleCall(the_body = body)
## obtain results.
pp$content$responses$faceAnnotations[[1]]

# Usando RoogleVision ----
sample_images=sample(1:length(imagenes), size = 500)
# ans=RoogleVision::getGoogleVisionResponse(paste0('imagenes_inmuebles_haya/',imagenes[1]), numResults = 50)

# Imagenes errores: 4576, 16315, 26500, 35671, 43728
#Tags=data.frame("mid"=c(),"description"=c(),"score"=c(),"topicality"=c())
for (i in 43728:length(imagenes)){
  ans=RoogleVision::getGoogleVisionResponse(paste0('imagenes_inmuebles_haya/',imagenes[i]), numResults = 50)
  ans$mid=c(imagenes[i])
  tryCatch(expr = {Tags <<- rbind(Tags, ans)},
           error=function(e){Tags <<- rbind(Tags, data.frame("mid"=c(NA),"description"=c(NA),"score"=c(NA),"topicality"=c(NA)))}) 
}

# Creando variables ####

Tags %>%
  group_by(description) %>%
  summarise(conteo=n()) %>%
  arrange(-conteo) %>% 
  head(26) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(description, conteo), y=conteo), stat = "identity")+
  coord_flip()

MostUsedTags = Tags %>%
  group_by(description) %>%
  summarise(conteo=n()) %>%
  arrange(-conteo) %>% 
  head(26) %>% .[,1, drop=T]

CleanTags=Tags[,1:3] %>%
  filter(!is.na(description) & description %in% MostUsedTags) %>%
  arrange(score) %>%
  filter(!duplicated.data.frame(.[,1:2])) %>% 
  spread(description, score) %>%
  rename(filename=mid)

bdImg=left_join(bdImg, CleanTags)

bdImg$posifoto=substr(bdImg$posifoto, 9,9) %>% as.numeric()


# image processing ####

#     PILA DE LENTO, NO CORRER

#Created Variables
# bdImg$mean_lightness=double(nrow(bdImg))
# bdImg$mean_color_r=double(nrow(bdImg))
# bdImg$mean_color_g=double(nrow(bdImg))
# bdImg$mean_color_b=double(nrow(bdImg))
# bdImg$color_variance=double(nrow(bdImg))
# bdImg$resolution=double(nrow(bdImg))
# 
# for (i in 4577:nrow(bdImg)){ #La imagen 4576 no es una imagen
#   am=magick::image_read(paste0('imagenes_inmuebles_haya/',imagenes[i]))
#   mean_lightness=image_channel(am, channel = "lightness") %>% 
#     image_data("rgb") %>%
#     as.integer(.)/255
#   bdImg$mean_lightness[i] = mean(mean_lightness[,,1])
#   mean_color=image_quantize(am, max = 1) %>% image_data("rgb")
#   mean_color=mean_color[,1,1] %>% as.integer()/255
#   bdImg$mean_color_r[i]=mean_color[1]
#   bdImg$mean_color_g[i]=mean_color[2]
#   bdImg$mean_color_b[i]=mean_color[3]
#   image_matrix= image_data(am,"rgb") %>% as.integer()/.255
#   s=0
#   placeholder<-apply(image_matrix, c(1,2), function(tuple){s<<-s+sum((tuple - min(tuple))^2)});
#   img_resolution=sqrt(prod(dim(image_matrix)[1:2]))
#   color_variance=sqrt(s)/img_resolution
#   
#   bdImg$resolution[i]=img_resolution
#   bdImg$color_variance[i]=color_variance
# }
# Prueba Perico
# am=image_read("C:/Users/ArmandoPC/Dropbox/Libreria/2do Trimestre/Aprendizaje no supervisado/Clustering jerarquico y no jerarquico/bird.jpg")

# Probando con parallel

library(foreach)
library(doParallel)

registerDoParallel(10) # Cantidad de núcleos a dedicar. No dedicar todos tus núcleos! Puedes ver cuantos tienes con parallel::detectCores()

ans<-foreach(i=1:nrow(bdImg),
        .combine = rbind,
        .packages = c('dplyr', 'magick'),
        .errorhandling = "remove") %dopar%
        {
          am=magick::image_read(paste0('imagenes_inmuebles_haya/',imagenes[i]))
          mean_lightness=image_channel(am, channel = "lightness") %>% 
            image_data("rgb") %>%
            as.integer(.)/255
          mean_lightness = mean(mean_lightness[,,1])
          mean_color=image_quantize(am, max = 1) %>% image_data("rgb")
          mean_color=mean_color[,1,1] %>% as.integer()/255
          mean_color_r=mean_color[1]
          mean_color_g=mean_color[2]
          mean_color_b=mean_color[3]
          image_matrix= image_data(am,"rgb") %>% as.integer()/.255
          s=0
          placeholder<-apply(image_matrix, c(1,2), function(tuple){s<<-s+sum((tuple - min(tuple))^2)});
          img_resolution=sqrt(prod(dim(image_matrix)[1:2]))
          color_variance=sqrt(s)/img_resolution
          
          data.frame("filename"=imagenes[i],
                     "mean_color_r"=mean_color_r,
                     "mean_color_g"=mean_color_g,
                     "mean_color_b"=mean_color_b,
                     "color_variance"=color_variance,
                     "resolution"=img_resolution,
                     "mean_lighness"=mean_lightness)
          }

stopImplicitCluster() # Cierra los procesos paralelos

bdImg=left_join(bdImg, ans)

fwrite(bdImg, "bdImg.csv")
