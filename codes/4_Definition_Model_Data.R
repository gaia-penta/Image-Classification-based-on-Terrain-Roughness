rm(list = ls())

current.path <- getwd()
# Workdirectory "Progetto"

# Librerie utilizzate -----------------------------------------------------
library(tidyverse)
library(flsa)
library(gridExtra)
library(data.table)
library(ggcorrplot)

# Carico SD_z_Acc e Z_acc -------------------------------------------------

Sd_z_Acc <- read.csv("Dataset/Y_Sd_Acc.csv")
# Deviazioni standard z Accelerometro su mille secondi

Z_Acc <- read.csv("Dataset/Z_Acc.csv")
# Misurazioni z Accelerometro per millisecodo


# Carico Record ---------------------------------------------------------------

Record <- bind_rows(read.csv("Dataset/record1.csv"),
                    read.csv("Dataset/record2.csv"),
                    read.csv("Dataset/record3.csv"),
                    read.csv("Dataset/record4.csv"),
                    read.csv("Dataset/record5.csv"))
# Informazioni aggiuntive di cui teniamo la velocitC 


# Carico SD_y_Gryo e Y_Gyro ---------------------------------------------------

Sd_y_Gyro <- read.csv("Dataset/Y_Sd_Gyro.csv")
# Deviazioni standard y Giroscopio su mille secondi

Y_Gyro <- read.csv("Dataset/Y_Gyro.csv")
# Misurazioni y Giroscopio per millisecondo


# Carico labels kaggle ----------------------------------------------------

labels1_kaggle <- read.csv("Dataset/tsm_1_labels.csv")
# Caricamento delle prime labels proposte sul sito kaggle

ID_images_in <- as.numeric(gsub("s.*","",labels1_kaggle$image))
# Identificativi dell'immagine, tolgo centesimi di secondo da id


# ___________ -------------------------------------------------------------
# Plot confronti ----------------------------------------------------------

ampiezze_giorni <- cumsum(c(167160,95370,479400,514830))
# Definisco ampiezze intervalli dei giorni sul totale delle rilevazioni z

ggplot(Z_Acc, aes(x = X,
                  y = x)) +
  geom_line() +
  geom_vline(xintercept = ampiezze_giorni, col = 2, lwd = 2) +
  xlab("Misurazioni totali effettuate nei 5 giorni") +
  ylab("Valori assunti dall'assasse Z dell'Accelerometro") +
  ggtitle("")
# Visualizzo tutte le rilevazioni relative all'asse Z dell'accelerometro

ampiezze_giorni <- cumsum(c(length(which(Sd_z_Acc$Day == 1)),
                            length(which(Sd_z_Acc$Day == 2)),
                            length(which(Sd_z_Acc$Day == 3)),
                            length(which(Sd_z_Acc$Day == 4))))
# l'ampiezza delle unitC  nei giorni cambia in quanto utilizzo sd

ggplot(Sd_z_Acc, aes(x = X, 
                     y = SD_z_Acc)) +
  geom_line() +
  geom_vline(xintercept = ampiezze_giorni, col = 2, lwd = 2) +
  xlab("Frame Totali") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
# Visualizzo l'andamento delle deviazioni standard nei 5 giorni

index_ID <- which(Sd_z_Acc$ID_Acc %in% ID_images_in)
Sd_z_Acc_Kaggle <- Sd_z_Acc[index_ID,]
ampiezze_giorni <- cumsum(c(length(which(Sd_z_Acc_Kaggle$Day == 1)),
                            length(which(Sd_z_Acc_Kaggle$Day == 2)),
                            length(which(Sd_z_Acc_Kaggle$Day == 3)),
                            length(which(Sd_z_Acc_Kaggle$Day == 4))))
# Ricercatori hanno suggerito un insieme ridotto di osservazioni poiche
# in alcune immagini non era presente la strada

ggplot(Sd_z_Acc_Kaggle, aes(x = 1:nrow(Sd_z_Acc_Kaggle), 
                            y = SD_z_Acc )) +
  geom_line() +
  geom_vline(xintercept = ampiezze_giorni, col = 2, lwd = 2) +
  xlab("Frame tenuti dai ricercatori di Kaggle") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
# Visualizzo solo le osservazioni nelle quali i ricercatori hanno decretato 
# si vedesse la strada

# __________ --------------------------------------------------------------
# Fused Lasso -------------------------------------------------------------

Y <- as.numeric(scale(Sd_z_Acc$SD_z_Acc, scale = F))
# Definisco una y dalle deviazioni standard centrate per Fused Lasso

fit <- flsa(Y)
# Stimo Fused lasso

# _______ -----------------------------------------------------------------
# __ K2 __ -------------------------------------------------------------------

ggplot() +
  geom_point(aes(x = 1:length(Y), y = Y)) +
  geom_line(aes(x = 1:length(Y),
                y = flsaGetSolution(fit, 
                                    lambda1 = 0.0025,
                                    lambda2 = 1)[1,1,]),
            col = "slateblue", lwd = 1) +
  geom_hline(yintercept = 0, lty = 2, col = 2) +
  xlab("Frame Totali") +
  ylab("Deviazione standard su Z dell'Accelerometro (Centrati)") +
  ggtitle(bquote(paste(lambda[1]==.(0.0025),
                       " ",
                       lambda[2]==.(1))))
# Visualizzo il comportamento del Fused lasso con parametro lasso pari a 
# 0.0025 e parametro di fusione 1

pred <- flsaGetSolution(fit, 
                        lambda1 = 0.000001,
                        lambda2 = 0.5)
# Dopo diverse prove si scelgono come parametro lasso 0.000001 e parametro di
# fusione 0.5

etichette2 <- rep(0,length(pred[1,1,]))
etichette2[pred[1,1,]>0] <- 2
etichette2[pred[1,1,]<0] <- 1
# Creo labels a due categorie 

ggplot() +
  geom_point(aes(x = 1:length(Y), y = Y), col = etichette2) +
  geom_line(aes(x = 1:length(Y),
                y = pred[1,1,]),
            col = "gold", lwd = 1) +
  geom_hline(yintercept = 0, lty = 2, col = 2) +
  xlab("Frame Totali") +
  ylab("Deviazione standard su Z dell'Accelerometro (Centrati)") +
  ggtitle(bquote(paste(lambda[1]==.(0.000001),
                       " ",
                       lambda[2]==.(0.5))))
# Visualizzo la divisione delle osservazioni secondo le 2 labels create con 
# Fused lasso in precedenza

# _______ -----------------------------------------------------------------
# __ K3 __ -------------------------------------------------------------------

ggplot() +
  geom_point(aes(x = 1:length(Y), y = Y)) +
  geom_line(aes(x = 1:length(Y),
                y = flsaGetSolution(fit, 
                                    lambda1 = 0.25,
                                    lambda2 = 1)[1,1,]),
            col = "slateblue", lwd = 1) +
  geom_hline(yintercept = 0, lty = 2, col = 2) +
  xlab("Frame Totali") +
  ylab("Deviazione standard su Z dell'Accelerometro (Centrati)") +
  ggtitle(bquote(paste(lambda[1]==.(0.25),
                       " ",
                       lambda[2]==.(1))))
# Visualizzo il comportamento del Fused lasso con parametro lasso pari a 
# 0.25 e parametro di fusione 1

pred <- flsaGetSolution(fit, 
                        lambda1 = 0.1,
                        lambda2 = 0.5)
# Dopo diverse prove si scelgono come parametro lasso 0.1 e parametro di
# fusione 0.5

etichette3 <- rep(2,length(pred))
etichette3[pred[1,1,]>0] <- 3
etichette3[pred[1,1,]<0] <- 1
# Creo labels a due categorie 

ggplot() +
  geom_point(aes(x = 1:length(Y), y = Y), col = etichette3) +
  geom_line(aes(x = 1:length(Y),
                y = pred[1,1,]),
            col = "gold", lwd = 1) +
  geom_hline(yintercept = 0, lty = 2, col = 2) +
  xlab("Frame Totali") +
  ylab("Deviazione standard su Z dell'Accelerometro (Centrati)") +
  ggtitle(bquote(paste(lambda[1]==.(0.1),
                       " ",
                       lambda[2]==.(0.5))))
# Visualizzo la divisione delle osservazioni secondo le 3 labels create con 
# Fused lasso in precedenza

# _______ ----------------------------------------------------------------
# K-Means -----------------------------------------------------------------

Data_cluster <- bind_cols(Sd_y_Gyro[,2:3],
                          SD_z_Acc = Sd_z_Acc[,"SD_z_Acc"])
names(Data_cluster)[1] <- "ID"
names(Record)[2] <- "ID"
Data_cluster <- right_join(Record[,c("ID","enhanced_speed..m.s.")],
                           Data_cluster,
                           by = "ID")
ind_NA_speed <- which(is.na(Data_cluster$enhanced_speed..m.s.))
media_speed <- mean(na.omit(Data_cluster$enhanced_speed..m.s.))
Data_cluster$enhanced_speed..m.s.[ind_NA_speed] <- media_speed
# Creo un dataset su cui applicare un cluster K-means, risultano 7
# valori relativi ai record sulla velocita a NA che vengono settati
# con la relativa media


# Esplorativa Cluster -------------------------------------------

ampiezze_giorni <- cumsum(c(167160,95370,479400,514830))
# Definisco ampiezze intervalli dei giorni sul totale delle rilevazioni y

ggplot(Y_Gyro, aes(x = X,
                   y = x)) +
  geom_line() +
  geom_vline(xintercept = ampiezze_giorni, col = 2, lwd = 2) +
  xlab("Misurazioni totali effettuate nei 5 giorni") +
  ylab("Valori assunti dall'assasse Y del Giroscopio") +
  ggtitle("")
# Visualizzo tutte le rilevazioni relative all'asse Z dell'accelerometro

ampiezze_giorni <- cumsum(c(length(which(Sd_z_Acc$Day == 1)),
                            length(which(Sd_z_Acc$Day == 2)),
                            length(which(Sd_z_Acc$Day == 3)),
                            length(which(Sd_z_Acc$Day == 4))))
# l'ampiezza delle unitC  nei giorni cambia in quanto utilizzo sd

ggplot(Sd_y_Gyro, aes(x = X, 
                      y = SD_y_Gyro)) +
  geom_line() +
  geom_vline(xintercept = ampiezze_giorni, col = 2, lwd = 2) +
  xlab("Frame Totali") +
  ylab("Deviazione standard su Y del Gyroscopio") +
  ggtitle("")
# Visualizzo l'andamento delle deviazioni standard nei 5 giorni

ggplot(Data_cluster, aes(x = 1:15020, 
                         y = enhanced_speed..m.s.)) +
  geom_line() +
  geom_vline(xintercept = ampiezze_giorni, col = 2, lwd = 2) +
  xlab("Frame Totali") +
  ylab("Variazione della velocitC ") +
  ggtitle("")
# Visualizzo l'andamento della velocitC  nei 5 giorni

corr <- round(cor(Data_cluster[,-1]), 1)
ggcorrplot(corr, hc.order = TRUE,
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"), lab=T)
# Visualizzo infine la corrrelazione tra SD_Z_Acc SD_Y_gyro e VelocitC 

# _______ -----------------------------------------------------------------
# __ K2 __ -------------------------------------------------------------------

Kmeans_k2 <- kmeans(scale(Data_cluster[,-1]), centers = 2)
# K-means con k = 2

# _______ -----------------------------------------------------------------
# __ K2 __ -------------------------------------------------------------------

Kmeans_k3 <- kmeans(scale(Data_cluster[,-1]), centers = 3)
# K-means con k = 3

# ___________ -------------------------------------------------------------
# Kaggle VS Fused VS Kmeans -----------------------------------------------

Sd_z_Acc$Etichette_Fused_K3 <- factor(etichette3)
Sd_z_Acc$Etichette_Fused_K2 <- factor(etichette2)
# Salvo per ogni immagine l'etichette trovate in precedenza con Fused Lasso


Sd_z_Acc$Etichette_Kmeans_K3 <- factor(Kmeans_k3$cluster)
Sd_z_Acc$Etichette_Kmeans_K2 <- factor(Kmeans_k2$cluster)
# Salvo per ogni immagine l'etichette trovate in precedenza con K-means


index_ID <- which(Sd_z_Acc$ID_Acc %in% ID_images_in)
Sd_z_Acc_Kaggle <- Sd_z_Acc[index_ID,]
# Seleziono solo quelle immagini selzionate dai ricercatori

data.help <- data.frame(ID_Acc = ID_images_in, 
                        Etichette_Kaggle_K2 = factor(labels1_kaggle$tsm1_k2),
                        Etichette_Kaggle_K3 = factor(labels1_kaggle$tsm1_k3))
Sd_z_Acc_Kaggle <- inner_join(Sd_z_Acc_Kaggle, data.help, by = "ID_Acc")
# Tramite un dataset di aiuto creo un dataset contenente le standard deviation
# sull'asse z dell'accelerometro e tutte le etichette da confrontare di seguito
# e da usare in fase di modellazione 

plot_Fused_k2 <- 
  ggplot(Sd_z_Acc_Kaggle, aes(x = 1:nrow(Sd_z_Acc_Kaggle), 
                              y = SD_z_Acc,
                              color = Etichette_Fused_K2)) +
  geom_point() +
  xlab("Frame tenuti dai ricercatori di Kaggle") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
plot_Kaggle_k2 <- 
  ggplot(Sd_z_Acc_Kaggle, aes(x = 1:nrow(Sd_z_Acc_Kaggle), 
                              y = SD_z_Acc,
                              color = Etichette_Kaggle_K2)) +
  geom_point() +
  xlab("Frame tenuti dai ricercatori di Kaggle") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
plot_Kmeans_k2 <- 
  ggplot(Sd_z_Acc_Kaggle, aes(x = 1:nrow(Sd_z_Acc_Kaggle), 
                              y = SD_z_Acc,
                              color = Etichette_Kmeans_K2)) +
  geom_point() +
  xlab("Frame tenuti dai ricercatori di Kaggle") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
# grid.arrange(plot_Kaggle_k2,
#              plot_Fused_k2,
#              plot_Kmeans_k2,
#              nrow = 3)
# Visualizzo in un unica finestra le etichette trovate tramite Fused Lasso,
# le etichette trovate con k-means le etichette date dai ricercatori su Kaggle,
# per k = 2

plot_Fused_k3 <-
  ggplot(Sd_z_Acc_Kaggle, aes(x = 1:nrow(Sd_z_Acc_Kaggle), 
                              y = SD_z_Acc,
                              color = Etichette_Fused_K3)) +
  geom_point() +
  xlab("Frame tenuti dai ricercatori di Kaggle") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
plot_Kaggle_k3 <- 
  ggplot(Sd_z_Acc_Kaggle, aes(x = 1:nrow(Sd_z_Acc_Kaggle), 
                              y = SD_z_Acc,
                              color = Etichette_Kaggle_K3)) +
  geom_point() +
  xlab("Frame tenuti dai ricercatori di Kaggle") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
plot_Kmeans_k3 <- 
  ggplot(Sd_z_Acc_Kaggle, aes(x = 1:nrow(Sd_z_Acc_Kaggle), 
                              y = SD_z_Acc,
                              color = Etichette_Kmeans_K3)) +
  geom_point() +
  xlab("Frame tenuti dai ricercatori di Kaggle") +
  ylab("Deviazione standard su Z dell'Accelerometro") +
  ggtitle("")
# grid.arrange(plot_Kaggle_k3,
#              plot_Fused_k3,
#              plot_Kmeans_k3,
#              ncol = 3)
# Visualizzo in un unica finestra le etichette trovate tramite Fused Lasso,
# le etichette trovate con k-means le etichette date dai ricercatori su Kaggle,
# per k = 3

# _____________ -----------------------------------------------------------
# Creo dataset per modellazione  ------------------------------------------

px1 <- fread("Dataset/Pixel_day1.csv")
names(px1)[2] <- "ID_images_in"
px2 <- fread("Dataset/Pixel_day2.csv")
names(px2)[2] <- "ID_images_in"
px3 <- fread("Dataset/Pixel_day3.csv")
names(px3)[2] <- "ID_images_in"
px4 <- fread("Dataset/Pixel_day4.csv")
names(px4)[2] <- "ID_images_in"
px5 <- fread("Dataset/Pixel_day5.csv")
names(px5)[2] <- "ID_images_in"
# Carico i dataset relativi ai pixxel delle immagini nei 5 diversi giorni
# cambio il nome dell'identificativo al fine di poter unire i pixel
# con le etichette trovate in precedenza

px_tot <- bind_rows(px1,
                    px2,
                    px3,
                    px4,
                    px5)
px_tot$V1 <- NULL
# Unisco tutti i pixxel di tutti e 5 i giorni in un unico dataset

Sd_z_Acc_Kaggle$X <- NULL
names(Sd_z_Acc_Kaggle)[1] <- "ID_images_in"
# Sistemo il dataset con le etichette e la standard deviation delle z 

id_outka <- which(duplicated(Sd_z_Acc_Kaggle$ID_images_in))
id_outpx <- which(duplicated(px_tot$ID_images_in))
# Ci si accorge che due frame hanno secondi ripetuti, in quanto per noi 
# l'identificativo corrispondeva al secondo del video, si isolano gli 
# identificativi ripetuti per poi aggiungerli di seguito, tale operazione
# C( dovuta al comportamento della funzione "inner_join" che ci permette 
# tramite un campo identificativo di aggregare due record di dataset distinti,
# se pero ci sono dei campi identificativi ripetuti, vengono considerati come 
# unici per tanto alla fine sarebbero state create 4 righe in piu di quelle che
# realmente ci sarebbero dovute essere

Sd_z_Acc_Kaggle2 <- Sd_z_Acc_Kaggle[-id_outka,]
px_tot2 <- px_tot[-id_outpx,]
Dati <- inner_join(Sd_z_Acc_Kaggle2, px_tot2, by = "ID_images_in")
Sd_z_Acc_Kaggle2 <- Sd_z_Acc_Kaggle[id_outka,]
px_tot2 <- px_tot[id_outpx,]
Dati2 <- inner_join(Sd_z_Acc_Kaggle2, px_tot2, by = "ID_images_in")
Dati <- bind_rows(Dati, Dati2)
# Si uniscono i due dataset creati, il primo contenente "ID_images_in" unici, 
# con lunghezza 7066, mentre il secondo contenente solo due valori, quelli 
# ripetuti

rm(list=setdiff(ls(), "Dati"))
# Rimuoviamo tutti gli oggetti dall'ambiente attuale

write.csv(Dati, file = "Dataset/Data_k2_k3_models.csv")
# Salvo il dataset finale nella WD "Dataset"











