rm(list = ls())

current.path <- getwd()
# Workdirectory "Progetto"

# _________ ---------------------------------------------------------------
# 2020-07-28 --------------------------------------------------------------

gyroscope <- read.csv("Dataset/gyroscope_calibrated_split1.csv", 
                          header=T)
# Scarico i dati relativi al giroscopio rilevati nella prima
# giornata 

Y_Gyro_Day1 <- gyroscope$calibrated_gyro_y..deg.s.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Gyro <- unique(gyroscope$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Gyro <- data.frame(ID_Gyro, SD_y_Gyro = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Gyro){
  
  index.giroscopio1 <- which(gyroscope$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(gyroscope$calibrated_gyro_y..deg.s.[index.giroscopio1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Gyro$ID_Gyro == i)
  Y_Sd_Gyro[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Data_Day1 <- Y_Sd_Gyro
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-09-23 --------------------------------------------------------------

gyroscope <- read.csv("Dataset/gyroscope_calibrated_split2.csv", 
                      header=T)
# Scarico i dati relativi al giroscopio rilevati nella seconda
# giornata 

Y_Gyro_Day2 <- gyroscope$calibrated_gyro_y..deg.s.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Gyro <- unique(gyroscope$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Gyro <- data.frame(ID_Gyro, SD_y_Gyro = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Gyro){
  
  index.giroscopio1 <- which(gyroscope$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(gyroscope$calibrated_gyro_y..deg.s.[index.giroscopio1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Gyro$ID_Gyro == i)
  Y_Sd_Gyro[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Data_Day2 <- Y_Sd_Gyro
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-09-24 --------------------------------------------------------------

gyroscope <- read.csv("Dataset/gyroscope_calibrated_split3.csv", 
                      header=T)
# Scarico i dati relativi al giroscopio rilevati nella terza
# giornata 

Y_Gyro_Day3 <- gyroscope$calibrated_gyro_y..deg.s.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Gyro <- unique(gyroscope$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Gyro <- data.frame(ID_Gyro, SD_y_Gyro = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Gyro){
  
  index.giroscopio1 <- which(gyroscope$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(gyroscope$calibrated_gyro_y..deg.s.[index.giroscopio1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Gyro$ID_Gyro == i)
  Y_Sd_Gyro[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Data_Day3 <- Y_Sd_Gyro
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-09-29 --------------------------------------------------------------

gyroscope <- read.csv("Dataset/gyroscope_calibrated_split4.csv", 
                      header=T)
# Scarico i dati relativi al giroscopio rilevati nella quarta
# giornata 

Y_Gyro_Day4 <- gyroscope$calibrated_gyro_y..deg.s.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Gyro <- unique(gyroscope$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Gyro <- data.frame(ID_Gyro, SD_y_Gyro = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Gyro){
  
  index.giroscopio1 <- which(gyroscope$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(gyroscope$calibrated_gyro_y..deg.s.[index.giroscopio1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Gyro$ID_Gyro == i)
  Y_Sd_Gyro[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Data_Day4 <- Y_Sd_Gyro
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-10-02 --------------------------------------------------------------

gyroscope <- read.csv("Dataset/gyroscope_calibrated_split5.csv", 
                      header=T)
# Scarico i dati relativi al giroscopio rilevati nella quinta
# giornata 

Y_Gyro_Day5 <- gyroscope$calibrated_gyro_y..deg.s.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Gyro <- unique(gyroscope$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Gyro <- data.frame(ID_Gyro, SD_y_Gyro = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Gyro){
  
  index.giroscopio1 <- which(gyroscope$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(gyroscope$calibrated_gyro_y..deg.s.[index.giroscopio1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Gyro$ID_Gyro == i)
  Y_Sd_Gyro[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Data_Day5 <- Y_Sd_Gyro
# Salvo il dataset da unire agli altri giorni


# ************* --------------------------------------------------------------
# Unione dati -------------------------------------------------------------


Data_final <- rbind(Data_Day1,
                    Data_Day2,
                    Data_Day3,
                    Data_Day4,
                    Data_Day5)
# Unisco in un unico dataset le standard deviation relative alle z 
# dell'accelerometro in un secondo per millisecondo

nome.dataset <- paste0(current.path,"/Dataset/Y_Sd_Gyro.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(Data_final, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"

Y_Gyro <- c(Y_Gyro_Day1,
            Y_Gyro_Day2,
            Y_Gyro_Day3,
            Y_Gyro_Day4,
            Y_Gyro_Day5)
# Unisco in un unico vettore i valori delle z dell'accelerometro per 
# millisecondo al fine di usarle per visualizzare in confronto alle relative
# standard deviation

nome.dataset <- paste0(current.path,"/Dataset/Y_Gyro.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(Y_Gyro, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"


# _________ ------------------------------------------------------------------










