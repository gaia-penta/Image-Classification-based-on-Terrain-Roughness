rm(list = ls())

current.path <- getwd()
# Workdirectory "Progetto"

# Librerie utilizzate -----------------------------------------------------

# install.packages("BiocManager")
# BiocManager::install("EBImage")
library(EBImage)
library(imager)

# Carico labels kaggle ----------------------------------------------------

labels1_kaggle <- read.csv("Dataset/tsm_1_labels.csv")
# Caricamento delle prime labels proposte sul sito kaggle

ID_images_in <- as.numeric(gsub("s.*","",labels1_kaggle$image))
# Identificativi dell'immagine, tolgo centesimi di secondo da id

# Immagine ----------------------------------------------------------------

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

plot(Image(Image_day1[,,,1]))
# Visualizzo immagine nella sua grandezza reale 

plot(Image(imager::resize(Image_day1, 
                          size_x = 190,
                          size_y = 340))[,,,1])
# Visualizzo immagine nella grandezza in cui è stata salvata
                   

# Accelerometro Day 1 ----------------------------------------------

path.acc <- paste0(current.path,"/Dataset/accelerometer_calibrated_split1.csv")
# Path per dati accelerometro relativi al primo giorno

accelerometer <- read.csv(path.acc, 
                          header=T)
# Scarico i dati relativi all'accelerometro rilevati nella prima
# giornata 

accelerometer$X <- NULL
# Cancello contatore di riga

ind.acc <- which(accelerometer$utc_s..s. == ID_image_day1)
# Prendo identificativo relativo all'immagine mostrata sopra

knitr::kable(head(accelerometer[ind.acc,]))
# Visualizzo dati relativi ad accelerometro per il frame mostratro sopra

# Gyroscopio Day 1 ------------------------------------------------------------

path.gyro <- paste0(current.path,"/Dataset/gyroscope_calibrated_split1.csv")
# Path per dati giroscopio relativi al primo giorno

gyroscope <- read.csv(path.gyro, 
                      header=T)
# Scarico i dati relativi al giroscopio rilevati nella prima
# giornata 

gyroscope$X <- NULL
# Cancello contatore di riga

ind.gyro <- which(gyroscope$utc_s..s. == ID_image_day1)
# Prendo identificativo relativo all'immagine mostrata sopra

knitr::kable(head(gyroscope[ind.gyro,]))
# Visualizzo dati relativi a giroscopio per il frame mostratro sopra

# Record Day 1 ----------------------------------------------------------------

path.reco <- paste0(current.path,"/Dataset/record1.csv")
# Path per dati record relativi al primo giorno

record <- read.csv(path.reco)
# Scarico i dati relativi ai record rilevati nella prima giornata 

record$X <- NULL
# Cancello contatore di riga

ind.reco <- which(record$utc_s..s. == ID_image_day1)
# Prendo identificativo relativo all'immagine mostrata sopra

knitr::kable(head(record[ind.reco,]))
# Visualizzo dati relativi a record per il frame mostratro sopra


