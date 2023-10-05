rm(list = ls())

current.path <- getwd()
# Workdirectory "Progetto"

# Librerie utilizzate -----------------------------------------------------

# install.packages("BiocManager")
# BiocManager::install("EBImage")
library(EBImage)
library(imager)
library(data.table)
library(flsa)
library(matrixcalc)

# Carico labels kaggle ----------------------------------------------------

labels1_kaggle <- read.csv("Dataset/tsm_1_labels.csv")
# Caricamento delle prime labels proposte sul sito kaggle

ID_images_in <- as.numeric(gsub("s.*","",labels1_kaggle$image))
# Identificativi dell'immagine, tolgo centesimi di secondo da id


# Carico Data Models ------------------------------------------------------

dati <- data.frame(fread("Dataset/Data_k2_k3_models.csv"))
# Caricamento dataset per modellazione 

# _____________ -----------------------------------------------------------
# Immagine media --------------------------------------------------------

medie.colonne <- colMeans(dati[,-c(1:10)])
# calcolo la media di ogni pixel per tutte le immagini

immagine.media <- matrix(as.numeric(medie.colonne),
                          byrow = T,
                          ncol = 340,
                          nrow = 190)
# Passo da vettore a matrice della media dei pixel

plot(Image(immagine.media))
# Visualizzo la media dei pixel

# _____________ -----------------------------------------------------------
# Fused Signal Approximator -----------------------------------------------

fit.FSA <- flsa(as.numeric(medie.colonne))
FSA.px <- as.vector(flsaGetSolution(fit.FSA, lambda1 = 0.2, lambda2 = 0.5))
# Fused lasso su tutte le medie dei pixel vettorizzati

immagine.FSA <- matrix(FSA.px,
                       byrow = T,
                       ncol = 340,
                       nrow = 190)
# Passo da vettore a matrice per i valori stimati dal fused sulla media
# dei pixel

fit.FSA.1 <- flsa(as.numeric(dati[1,-c(1:10)]))
FSA.px.1 <- as.vector(flsaGetSolution(fit.FSA.1, lambda1 = 0.2, lambda2 = 0.5))
immagine.FSA.1 <- matrix(FSA.px.1,
                         byrow = T,
                         ncol = 340,
                         nrow = 190)
# Riproduco quanto fatto in precedenza per la prima immagine

par(mfrow = c(1,2))
plot(Image(immagine.FSA))
# Visualizzo Fused Signal Approximator su pixel medii

plot(Image(immagine.FSA.1))
# Visualizzo Fused Signal Approximator su pixel prima immagine

par(mfrow = c(1,1))

# Creo Dataset Fused ------------------------------------------------------


data.mat <- matrix(NA,
                   ncol = ncol(dati[,-c(1:10)]),
                   nrow = nrow(dati))
# Inizializzo una matrice vuota per caricarci successivamente 
# i pixel denoisati dal fused

for(i in 1:nrow(dati)){
  fit <- flsa(as.numeric(dati[i,-c(1:10)]))
  data.mat[i,] <- as.vector(flsaGetSolution(fit,
                                            lambda1 = 0.2,
                                            lambda2 = 0.5))
}
# Data l'impossibilità di valutare singolarmente ogni immagine si utilizzano
# i parametri utilizzati sopra per applicare denoise



# _____________ -----------------------------------------------------------
# Immagine ridotta -----------------------------------------------------------

path_image_day1 <- "/Dataset/Images/Images/2020-07-28"
setwd(paste0(current.path,path_image_day1))
# Set WD nel percorso della cartella contenente le immagini della prima
# giornata 

k = 0
# Nel caso presente siamo interessati a visualizzare una sola immagine
# precisamente la prima appartenente all'insieme di immagini utilizzate 
# dai ricercatori di kaggle, a tal proposito si utilizza un contatore
# che ci permetta di uscire dalla cartella delle immagini del primo giorno
# una volta trovata la prima immagine 

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_image_day1 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_image_day1 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente
    
    Image_day1 <- load.image(immagine)
    # Salvo immagine di interesse
    
    k = k +1
    # Porto avanti il contatore
  }
  
  if(k>0) break
  # Esco dal ciclo una volta presa la prima immagine
  
}

par(mfrow=c(1,2))
plot(Image(imager::resize(Image_day1, 
                          size_x = 200,
                          size_y = 340))[,,,1])
# Visualizzo immagine nella grandezza in cui è stata salvata

plot(Image(imager::resize(Image_day1, 
                          size_x = 200,
                          size_y = 340))[,,,1])
polygon(c(0,49,49,0),c(0,0,340,340),col = 1)
polygon(c(150,200,200,150),c(0,0,340,340),col = 1)
polygon(c(0,200,200,0),c(0,0,170,170),col = 1)
abline(v = 49, col = 3, lwd = 4)
abline(v = 150, col = 3, lwd = 4)
segments(0,170,200,170,col = 4,lwd = 4)
# Evidenzio la parte dell'immagine che andremo a tenere
par(mfrow = c(1,1))


# Creo Dataset Imagini ridotte --------------------------------------------
data <- matrix(NA,
               ncol = 17100,
               nrow = 7068)
ID_images_day_in <- numeric(7068)
# Inizializzo sia una matrice vuota dove salvare i valori dei pixel vettorizzati
# sia un vettore vuoto dove salvare i corrispondenti identificativi

# _________ ---------------------------------------------------------------
# 2020-07-28 --------------------------------------------------------------

path_image_day1 <- "/Dataset/Images/Images/2020-07-28"
setwd(paste0(current.path,path_image_day1))
# Set WD nel percorso della cartella contenente le immagini della prima
# giornata

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  

  ID_images_day1 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day1 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente

    im <- load.image(immagine)
    im <- imager::resize(im, size_x = 200, size_y = 340)
    data[i,] <- matrixcalc::vec(t(im[-c(1:49,150:200),170:340,,1]))[,1]
    # Salvo per ogni immagine 100*171 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day_in[i] <- ID_images_day1
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

# _________ ---------------------------------------------------------------
# 2020-09-23 --------------------------------------------------------------

path_image_day2 <- "/Dataset/Images/Images/2020-09-23"
setwd(paste0(current.path,path_image_day2))
# Set WD nel percorso della cartella contenente le immagini della seconda
# giornata 

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day1 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day1 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente
    
    im <- load.image(immagine)
    im <- imager::resize(im, size_x = 200, size_y = 340)
    data[i,] <- matrixcalc::vec(t(im[-c(1:49,150:200),170:340,,1]))[,1]
    # Salvo per ogni immagine 100*171 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day_in[i] <- ID_images_day1
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

# _________ ---------------------------------------------------------------
# 2020-09-24 --------------------------------------------------------------

path_image_day3 <- "/Dataset/Images/Images/2020-09-24"
setwd(paste0(current.path,path_image_day3))
# Set WD nel percorso della cartella contenente le immagini della terza
# giornata 
for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day1 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day1 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente

    im <- load.image(immagine)
    im <- imager::resize(im, size_x = 200, size_y = 340)
    data[i,] <- matrixcalc::vec(t(im[-c(1:49,150:200),170:340,,1]))[,1]
    # Salvo per ogni immagine 100*171 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day_in[i] <- ID_images_day1
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

# _________ ---------------------------------------------------------------
# 2020-09-29 --------------------------------------------------------------

path_image_day4 <- "/Dataset/Images/Images/2020-09-29"
setwd(paste0(current.path,path_image_day4))
# Set WD nel percorso della cartella contenente le immagini della quarta
# giornata 
for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 

  ID_images_day1 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day1 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente

    im <- load.image(immagine)
    im <- imager::resize(im, size_x = 200, size_y = 340)
    data[i,] <- matrixcalc::vec(t(im[-c(1:49,150:200),170:340,,1]))[,1]
    # Salvo per ogni immagine 100*171 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day_in[i] <- ID_images_day1
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

# _________ ---------------------------------------------------------------
# 2020-10-02 --------------------------------------------------------------


path_image_day5 <- "/Dataset/Images/Images/2020-10-02"
setwd(paste0(current.path,path_image_day5))
# Set WD nel percorso della cartella contenente le immagini della quinta
# giornata 

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day1 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day1 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente

    im <- load.image(immagine)
    im <- imager::resize(im, size_x = 200, size_y = 340)
    data[i,] <- matrixcalc::vec(t(im[-c(1:49,150:200),170:340,,1]))[,1]
    # Salvo per ogni immagine 100*171 pixxel vettorizzati come vettori
    # riga
    
    
    ID_images_day_in[i] <- ID_images_day1
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

# _________ ---------------------------------------------------------------
# Salvo risultati ---------------------------------------------------------

nome.dataset.matrice.fused <- paste0(current.path,"/Dataset/Matrice_fused.csv")
nome.dataset.pixel.piccoli <- paste0(current.path,"/Dataset/Pixel_piccoli.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(data.mat,
          file = nome.dataset.matrice.fused)
# Salvo i pixel denoisati

write.csv(data, 
          file = nome.dataset.pixel.piccoli)
# Salvo dati in Directory "Progetto"




