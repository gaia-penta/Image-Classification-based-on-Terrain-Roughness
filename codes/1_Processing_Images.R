rm(list = ls())

current.path <- getwd()
# Workdirectory "Progetto"

# Librerie utilizzate -----------------------------------------------------

library(imager)
library(matrixcalc)


# Carico labels kaggle ----------------------------------------------------

labels1_kaggle <- read.csv("Dataset/tsm_1_labels.csv")
# Caricamento delle prime labels proposte sul sito kaggle

ID_images_in <- as.numeric(gsub("s.*","",labels1_kaggle$image))
# Identificativi dell'immagine, tolgo centesimi di secondo da id

# _________ ---------------------------------------------------------------
# 2020-07-28 --------------------------------------------------------------


path_image_day1 <- "/Dataset/Images/Images/2020-07-28"
setwd(paste0(current.path,path_image_day1))
# Set WD nel percorso della cartella contenente le immagini della prima
# giornata 

data <- NULL
ID_images_day1_in <- NULL
# Inizializzo data per salvare i pixel, e l'identificativo delle immagini
# utilizzate in fase di modellazione

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day1 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day1 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente
    
    im <- load.image(immagine)
    im <- resize(im, size_x = 190, size_y = 340)
    data <- rbind(data,t(matrixcalc::vec(t(im[,,,1]))))
    # Salvo per ogni immagine 190*340 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day1_in <- c(ID_images_day1_in, ID_images_day1)
    # Salvo identificativo delle immagini che sono tenute

  }
  
}

dati <- cbind(ID_images_day1_in, data)
# Unisco pixel immagini a relativo identificativo dell'immagine

nome.dataset <- paste0(current.path,"/Dataset/Pixel_day1.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(dati, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"

# _________ ---------------------------------------------------------------
# 2020-09-23 --------------------------------------------------------------

path_image_day2 <- "/Dataset/Images/Images/2020-09-23"
setwd(paste0(current.path,path_image_day2))
# Set WD nel percorso della cartella contenente le immagini della seconda
# giornata 

data <- NULL
ID_images_day2_in <- NULL
# Inizializzo data per salvare i pixel, e l'identificativo delle immagini
# utilizzate in fase di modellazione

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day2 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day2 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente
    
    im <- load.image(immagine)
    im <- resize(im, size_x = 190, size_y = 340)
    data <- rbind(data,t(matrixcalc::vec(t(im[,,,1]))))
    # Salvo per ogni immagine 190*340 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day2_in <- c(ID_images_day2_in, ID_images_day2)
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

dati <- cbind(ID_images_day2_in, data)
# Unisco pixel immagini a relativo identificativo dell'immagine

nome.dataset <- paste0(current.path,"/Dataset/Pixel_day2.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(dati, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"

# _________ ---------------------------------------------------------------
# 2020-09-24 --------------------------------------------------------------

path_image_day3 <- "/Dataset/Images/Images/2020-09-24"
setwd(paste0(current.path,path_image_day3))
# Set WD nel percorso della cartella contenente le immagini della terza
# giornata 

data <- NULL
ID_images_day3_in <- NULL
# Inizializzo data per salvare i pixel, e l'identificativo delle immagini
# utilizzate in fase di modellazione

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day3 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day3 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente
    
    im <- load.image(immagine)
    im <- resize(im, size_x = 190, size_y = 340)
    data <- rbind(data,t(matrixcalc::vec(t(im[,,,1]))))
    # Salvo per ogni immagine 190*340 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day3_in <- c(ID_images_day3_in, ID_images_day3)
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

dati <- cbind(ID_images_day3_in, data)
# Unisco pixel immagini a relativo identificativo dell'immagine

nome.dataset <- paste0(current.path,"/Dataset/Pixel_day3.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(dati, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"

# _________ ---------------------------------------------------------------
# 2020-09-29 --------------------------------------------------------------

path_image_day4 <- "/Dataset/Images/Images/2020-09-29"
setwd(paste0(current.path,path_image_day4))
# Set WD nel percorso della cartella contenente le immagini della quarta
# giornata 

data <- NULL
ID_images_day4_in <- NULL
# Inizializzo data per salvare i pixel, e l'identificativo delle immagini
# utilizzate in fase di modellazione

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day4 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day4 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente
    
    im <- load.image(immagine)
    im <- resize(im, size_x = 190, size_y = 340)
    data <- rbind(data,t(matrixcalc::vec(t(im[,,,1]))))
    # Salvo per ogni immagine 190*340 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day4_in <- c(ID_images_day4_in, ID_images_day4)
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

dati <- cbind(ID_images_day4_in, data)
# Unisco pixel immagini a relativo identificativo dell'immagine

nome.dataset <- paste0(current.path,"/Dataset/Pixel_day4.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(dati, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"

# _________ ---------------------------------------------------------------
# 2020-10-02 --------------------------------------------------------------


path_image_day5 <- "/Dataset/Images/Images/2020-10-02"
setwd(paste0(current.path,path_image_day5))
# Set WD nel percorso della cartella contenente le immagini della quinta
# giornata 

data <- NULL
ID_images_day5_in <- NULL
# Inizializzo data per salvare i pixel, e l'identificativo delle immagini
# utilizzate in fase di modellazione

for (immagine in list.files()){
  # Ciclo per tutte le immagini contenute nella cartella 
  
  ID_images_day5 <- as.numeric(gsub("s.*","",substr(immagine, 1,15)))
  # Salvo l'identificativo dell'immagine corrente 
  
  if(ID_images_day5 %in% ID_images_in){
    # Specifico condizione per la quale sono interessato a salvare i 
    # pixxel dell'immagine corrente
    
    im <- load.image(immagine)
    im <- resize(im, size_x = 190, size_y = 340)
    data <- rbind(data,t(matrixcalc::vec(t(im[,,,1]))))
    # Salvo per ogni immagine 190*340 pixxel vettorizzati come vettori
    # riga
    
    ID_images_day5_in <- c(ID_images_day5_in, ID_images_day5)
    # Salvo identificativo delle immagini che sono tenute
    
  }
  
}

dati <- cbind(ID_images_day5_in, data)
# Unisco pixel immagini a relativo identificativo dell'immagine

nome.dataset <- paste0(current.path,"/Dataset/Pixel_day5.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(dati, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"




















