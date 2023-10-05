rm(list = ls())

current.path <- getwd()
# Workdirectory "Progetto"

# _________ ---------------------------------------------------------------
# 2020-07-28 --------------------------------------------------------------

accelerometer <- read.csv("Dataset/accelerometer_calibrated_split1.csv", 
                          header=T)
# Scarico i dati relativi all'accelerometro rilevati nella prima
# giornata 

Z_Acc_Day1 <- accelerometer$calibrated_accel_z..g.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Acc <- unique(accelerometer$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Acc <- data.frame(ID_Acc, SD_z_Acc = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Acc){
  
  index.accelerometro1 <- which(accelerometer$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(accelerometer$calibrated_accel_z..g.[index.accelerometro1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Acc$ID_Acc == i)
  Y_Sd_Acc[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Y_Sd_Acc$Day <- rep(1, nrow(Y_Sd_Acc))
# Aggiungo un identificativo relativo al giorno

Data_Day1 <- Y_Sd_Acc
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-09-23 --------------------------------------------------------------

accelerometer <- read.csv("Dataset/accelerometer_calibrated_split2.csv", 
                          header=T)
# Scarico i dati relativi all'accelerometro rilevati nella seconda
# giornata 

Z_Acc_Day2 <- accelerometer$calibrated_accel_z..g.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Acc <- unique(accelerometer$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Acc <- data.frame(ID_Acc, SD_z_Acc = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Acc){
  
  index.accelerometro1 <- which(accelerometer$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(accelerometer$calibrated_accel_z..g.[index.accelerometro1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Acc$ID_Acc == i)
  Y_Sd_Acc[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Y_Sd_Acc$Day <- rep(2, nrow(Y_Sd_Acc))
# Aggiungo un identificativo relativo al giorno

Data_Day2 <- Y_Sd_Acc
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-09-24 --------------------------------------------------------------

accelerometer <- read.csv("Dataset/accelerometer_calibrated_split3.csv", 
                          header=T)
# Scarico i dati relativi all'accelerometro rilevati nella terza
# giornata 

Z_Acc_Day3 <- accelerometer$calibrated_accel_z..g.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Acc <- unique(accelerometer$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Acc <- data.frame(ID_Acc, SD_z_Acc = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Acc){
  
  index.accelerometro1 <- which(accelerometer$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(accelerometer$calibrated_accel_z..g.[index.accelerometro1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Acc$ID_Acc == i)
  Y_Sd_Acc[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Y_Sd_Acc$Day <- rep(3, nrow(Y_Sd_Acc))
# Aggiungo un identificativo relativo al giorno

Data_Day3 <- Y_Sd_Acc
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-09-29 --------------------------------------------------------------

accelerometer <- read.csv("Dataset/accelerometer_calibrated_split4.csv", 
                          header=T)
# Scarico i dati relativi all'accelerometro rilevati nella quarta
# giornata 

Z_Acc_Day4 <- accelerometer$calibrated_accel_z..g.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Acc <- unique(accelerometer$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Acc <- data.frame(ID_Acc, SD_z_Acc = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Acc){
  
  index.accelerometro1 <- which(accelerometer$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(accelerometer$calibrated_accel_z..g.[index.accelerometro1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Acc$ID_Acc == i)
  Y_Sd_Acc[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Y_Sd_Acc$Day <- rep(4, nrow(Y_Sd_Acc))
# Aggiungo un identificativo relativo al giorno

Data_Day4 <- Y_Sd_Acc
# Salvo il dataset da unire agli altri giorni

# _________ ---------------------------------------------------------------
# 2020-10-02 --------------------------------------------------------------


accelerometer <- read.csv("Dataset/accelerometer_calibrated_split5.csv", 
                          header=T)
# Scarico i dati relativi all'accelerometro rilevati nella quinta
# giornata 

Z_Acc_Day5 <- accelerometer$calibrated_accel_z..g.
# Salvo le misurazioni totali da riutilizzare alla fine come confronto

ID_Acc <- unique(accelerometer$utc_s..s.)
# Salvo ID relativi ai secondi per ogni millisecondo, per ricondurmi
# all'identificativo dell'immagine

Y_Sd_Acc <- data.frame(ID_Acc, SD_z_Acc = NA)
# Inizializzo un dataset per ogni immagine della relativa giornata,
# e una colonna di NA ai quali associare la standard deviation tra le
# le registrazioni per millisecondo all'interno di un secondo

for(i in ID_Acc){
  
  index.accelerometro1 <- which(accelerometer$utc_s..s. == i)
  # Identuficativo del secondo corrente 
  
  var.ima.zc <- sd(accelerometer$calibrated_accel_z..g.[index.accelerometro1])
  # Calco standard deviation relativa al secondo
  
  index.var <- which(Y_Sd_Acc$ID_Acc == i)
  Y_Sd_Acc[index.var, 2] <- var.ima.zc
  # Alloco standard deviation calcolata al secondo corrispondente
  
  
}

Y_Sd_Acc$Day <- rep(5, nrow(Y_Sd_Acc))
# Aggiungo un identificativo relativo al giorno

Data_Day5 <- Y_Sd_Acc
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

nome.dataset <- paste0(current.path,"/Dataset/Y_Sd_Acc.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(Data_final, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"

Z_Acc <- c(Z_Acc_Day1,
           Z_Acc_Day2,
           Z_Acc_Day3,
           Z_Acc_Day4,
           Z_Acc_Day5)
# Unisco in un unico vettore i valori delle z dell'accelerometro per 
# millisecondo al fine di usarle per visualizzare in confronto alle relative
# standard deviation

nome.dataset <- paste0(current.path,"/Dataset/Z_Acc.csv")
# Definisco dove e con che nome salvare i dati appena creati

write.csv(Z_Acc, 
          file = nome.dataset)
# Salvo dati in Directory "Progetto"


# _________ ------------------------------------------------------------------

