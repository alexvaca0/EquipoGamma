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

# image processing ####
#a=readJPEG(paste0('imagenes_inmuebles_haya/',imagenes[1]))
am=magick::image_read(paste0('imagenes_inmuebles_haya/',imagenes[1]))
# dims: [height, width, rgb]
lightness=image_channel(am, channel = "lightness")
lightness_mat=image_data(lightness, "rgb") %>% as.integer()/255
mean_lighness=mean(lightness_mat[1,,])

a2=image_quantize(a2, max = 5) # Los 5 colores mas predominantes

a3=image_data(img)

img <- readJPEG(paste0('imagenes_inmuebles_haya/',imagenes[1])) 
img2=image_read(img)
print(img2)
matimg=image_data(img2, 'rgb')
matimg=img2[[1]]


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

ºTags %>%
  group_by(description) %>%
  summarise(conteo=n()) %>%
  arrange(-conteo) %>% 
  head(50) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(description, conteo), y=conteo), stat = "identity")+
  coord_flip()





