rm(list = ls())
setwd("C:/Users/gaiap/OneDrive/Desktop/Università/2022-2023/Elevata Dimensionalità/Progetto")
load("Environment/Modelli_Fused_def1.RData")

current.path <- getwd()
# Workdirectory "Progetto"

# Librerie utilizzate -----------------------------------------------------

library(data.table)
library(glmnet)
library(sparseSVM)
library(ncvreg)
library(pamr)
library(tidyverse)
require(doMC)

# Carico dati --------------------------------------------------------

dati <- data.frame(fread("Dataset/Data_k2_k3_models.csv"))
# carico il dataset con i pixel presi inizialmente

id.test <- which(dati$Day == 3)
# Definisco di usare come insieme di verifica il terzo giorno 

train <- dati[-id.test,]
test <- dati[id.test,]
# Creo insieme di stima e insieme di verifica

rm(dati)
# Cancello i dati dall'ambiente attuale cosi da avere meno memoria occupata

# _____________ -----------------------------------------------------------
# Definizione Metriche ----------------------------------------------------


tabella.sommario <- function(previsti, osservati){
  n <-  table(previsti,osservati)
  err.tot <- 1-sum(diag(n))/sum(n)
  fn <- n[1,2]/(n[1,2]+n[2,2])
  fp <- n[2,1]/(n[1,1]+n[2,1])
  print(n)
  cat("errore totale: ", format(err.tot),"\n")
  cat("falsi positivi & falsi negativi: ",format(c(fp, fn)),"\n")
  invisible(n)
}
# Funzione per calcolare tabella di Errata-Classificazione

indici.errore<-function(tabella){
  Errata.Classificazione<- round((tabella[2]+tabella[3])/sum(tabella),3)
  Accuratezza<- round((tabella[1]+tabella[4])/sum(tabella),3)
  Sensibilità<- round((tabella[4]/sum(tabella[3],tabella[4])),3)
  Specificità<- round((tabella[1]/sum(tabella[1],tabella[2])),3)
  alpha <- round((tabella[2]/sum(tabella[1],tabella[2])),3)
  beta <- round((tabella[3]/sum(tabella[3],tabella[4])),3)
  False.Disc.Rate<- round(alpha/(1+alpha-beta),3)
  Precisione <-round((1-beta)/(1+alpha-beta),3)
  F1.Score<- round(2*tabella[4]/(2*tabella[4]+tabella[2]+tabella[3]),3)
  
  False.positive.rate <- alpha
  False.negative.rate <- beta
  
  indici<-data.frame(Errata.Classificazione,
                     Accuratezza,
                     Sensibilità,
                     Specificità,
                     False.positive.rate,
                     False.negative.rate,
                     False.Disc.Rate,
                     Precisione,
                     F1.Score)
  return(indici)
}
# Funzione per ricavare varie metriche dalla tabella di Errata-Classificazione

# _____________ -----------------------------------------------------------

set.seed(42)
fold <- sample(5, nrow(train), replace = T)
# Per coerenza definisco dei fold uguali per tutti i modelli che verranno
# stimati in CV

X_train <- as.matrix(train[,-c(1:10)])
X_test <- as.matrix(test[,-c(1:10)])
# Poichè la maggior parte delle funzioni per modellare richiede che la matrice
# del disegno sia un oggetto matrice converto gli insiemi di stima e verifica 
# in matrici



# Lasso Logistico Fused -------------------------------------------------------------

lambda.grid.lasso<- exp(seq(-5,-2.5,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia è il risultato di prove precedenti che
# non vengono riportate

registerDoMC(cores = 5)
# Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
# che intendo utilizzare per parallelizzare il modello 

Lasso_logistico_Fused <- cv.glmnet(X_train,
                                   factor(train$Etichette_Fused_K2),
                                   type.measure = "class",
                                   family = "binomial",
                                   alpha = 1,
                                   nfolds = 5,
                                   foldid = fold,
                                   lambda = lambda.grid.lasso,
                                   parallel = T,
                                   trace.it = 1,
                                   seed = 42)
# Viene stimato un modello logistico con penalita lasso, in CV a 5 fold con 
# metrica di ottimizzazione l'errore di errata classificazione,
# viene oltretutto fissato un seed per la riproducibilità dei risultati

plot(Lasso_logistico_Fused)
# Visualizzo andamento Errore di classificazione per i vari lambda

n.coef_Lasso_Fused <- length(which(coef(Lasso_logistico_Fused, s=Lasso_logistico_Fused$lambda.min)!=0))
# Numero di coefficienti diversi da 0 con lambda minimo


y.hat.lasso_Fused <- predict(Lasso_logistico_Fused, 
                             s = Lasso_logistico_Fused$lambda.min, 
                             newx = X_test, 
                             type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.lasso_Fused <- tabella.sommario(test$Etichette_Fused_K2 , 
                                        y.hat.lasso_Fused)
Metriche_Lasso_Fused <- indici.errore(Tabella.lasso_Fused)
# Salvo le varie metriche ottenute da tale modello 


# Elastic net Logistico Fused Lasso -------------------------------------------------------
lambda.grid.enet <- exp(seq(-5,-2.5,l=100))
# Come in precedenza si tiene una griglia per lambda ristretta cosi venga 
# esplorato un sottoinsieme di valori ragionevoli 

alpha.grid.enet <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
error.alpha.enet <- rep(NA, length(alpha.grid))
se.alpha.enet <- rep(NA, length(alpha.grid))
# Per far si che il modello abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializzano
# due vettori che conterranno l'errore di classificazione medio e la relativa
# deviazione standard

for(i in 1:length(alpha.grid.enet)){
  
  registerDoMC(cores = 5)
  # Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
  # che intendo utilizzare per parallelizzare il modello
  
  fit <- cv.glmnet(X_train,
                   factor(train$Etichette_Fused_K2),
                   alpha = alpha.grid.enet[i],
                   type.measure = "class",
                   family = "binomial",
                   nfolds = 5,
                   foldid = fold,
                   lambda = lambda.grid,
                   parallel = T,
                   trace.it = 1,
                   seed = 42)
  # Viene stimato un modello logistico con penalita Elastic-Net,
  # in CV a 5 fold con metrica di ottimizzazione l'errore di errata
  # classificazione, per ogni valore di alpha contenuto in alpha.grid,
  # viene oltretutto fissato un seed per la riproducibilità dei risultati
  
  error.alpha.enet[i] <- fit$cvm[fit$index["min",]]
  se.alpha.enet[i] <- fit$cvsd[fit$index["min",]]
  # La procedura valuta per ogni modello di parametro alpha_i il miglior 
  # errore con standard deviation associata, per poi essere tra loro confrontati
  
}

stderrcv.k.enet <- se.alpha.enet
`Misclassification Error` <-  error.alpha.enet
`Alpha` <- alpha.grid.enet
data.plot.cv.enet <- tibble(`Misclassification Error`,`Alpha`)
# Vengono salvati i risultati ottenuti per i diversi alpha al fine di riprodurre
# un grafico per l'andamento dell'errore 

srs.acc_ENet_Fused <- ggplot(data.plot.cv.enet, 
                        mapping = aes(x = `Alpha`, 
                                      y = `Misclassification Error`)) +
  ylim(c(min(`Misclassification Error` - stderrcv.k.enet)-0.005,
         max(`Misclassification Error` + stderrcv.k.enet)+0.005))+
  geom_point(aes(x = `Alpha`, y = `Misclassification Error` + stderrcv.k.enet),
             shape = 95, size = 5) +
  geom_point(aes(x = `Alpha`, y = `Misclassification Error` - stderrcv.k.enet),
             shape = 95, size = 5) +
  geom_segment(aes(x = `Alpha`, y = `Misclassification Error` - stderrcv.k.enet,
                   xend = `Alpha`, 
                   yend = `Misclassification Error` + stderrcv.k.enet)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Misclassification Error CV - Fused Lasso Labels")
srs.acc_ENet_Fused
# Visualizzazione dell'errore di classificazione per i diversi alpha 

error.alpha.enet[which.min(error.alpha.enet)]
# Minimo dell'errore di classificazione al variare dell'alpha

best.alpha_Fused <- alpha.grid.enet[which.min(error.alpha.enet)]
best.alpha_Fused

# Viene definito l'alpha per il quale si ottiene l'errore di classificazione 
# minore

E.Net_logistico_Fused <- cv.glmnet(X_train,
                                   factor(train$Etichette_Fused_K2),
                                   type.measure = "class",
                                   family = "binomial",
                                   alpha = best.alpha_Fused,
                                   nfolds = 5,
                                   foldid = fold,
                                   lambda = lambda.grid,
                                   parallel = T,
                                   trace.it = 1,
                                   seed = 42)
# Viene ristimato il modello logistico con penalita Elastic-Net fissata dal 
# best.alpha_Fused, in CV a 5 fold con metrica di ottimizzazione l'errore di 
# errata classificazione, per ogni valore di alpha contenuto in alpha.grid,
# viene oltretutto fissato un seed per la riproducibilità dei risultati

plot(E.Net_logistico_Fused)
# Visualizzo l'andamento dell'errore di classificazione per i diversi lambda
# del modello Elastic-Net

n.coef_E.Net_logistico_Fused <- length(which(coef(E.Net_logistico_Fused, s=E.Net_logistico_Fused$lambda.min)!=0))
# Numero di coefficienti diversi da 0 con lambda.min

y.hat.E.Net_Fused <- predict(E.Net_logistico_Fused, 
                             s = E.Net_logistico_Fused$lambda.min, 
                             newx = X_test, 
                             type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.E.Net_Fused <- tabella.sommario(test$Etichette_Fused_K2 , 
                                        y.hat.E.Net_Fused)
Metriche_E.Net_Fused <- indici.errore(Tabella.E.Net_Fused)
# Salvo le varie metriche ottenute da tale modello 


# SCAD --------------------------------------------------------------------

lambda.grid.scad <- exp(seq(-3.8,-4.2,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia è il risultato di prove precedenti che
# non vengono riportate

SCAD_logistico_Fused <- cv.ncvreg(X_train,
                                  factor(train$Etichette_Fused_K2),
                                  family = "binomial",
                                  penalty = "SCAD",
                                  nfolds = 5,
                                  fold = fold,
                                  trace = T,
                                  lambda = lambda.grid.scad,
                                  seed = 42)
# Viene stimato un modello logistico con penalita SCAD, in CV a 5 fold con 
# metrica di ottimizzazione la verosimiglianza negativa binomiale,
# viene oltretutto fissato un seed per la riproducibilità dei risultati

n.coef_SCAD_Fused <- SCAD_logistico_Fused$min
# Numero di coefficienti diversi da 0


SCAD_logistico_Fused$lambda.min

plot(SCAD_logistico_Fused)
# Visualizzo andamento della verosimiglianza negativa per i vari lambda


SCAD_logistico_Fused$lambda.min

SCAD_logistico_Fused.fit <- ncvreg(X_train,
                                  factor(train$Etichette_Fused_K2),
                                  family = "binomial",
                                  penalty = "SCAD",
                                  #lambda = lambda.grid, 
                                  trace=T)

plot(SCAD_logistico_Fused.fit)
abline(v=SCAD_logistico_Fused$lambda.min)
# Grafico 

SCAD_logistico_Fused.fit$lambda
SCAD_logistico_Fused.fit$lambda[SCAD_logistico_Fused.fit$convex.min]
# Valore limite di lambda prima che la funzione diventi non convessa

SCAD_logistico_Fused$lambda.min

y.hat.SCAD_Fused <- predict(SCAD_logistico_Fused, 
                            X_test, 
                            s = SCAD_logistico_Fused$lambda.min, 
                            type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.SCAD_Fused <- tabella.sommario(test$Etichette_Fused_K2 ,
                                       y.hat.SCAD_Fused)
Metriche_SCAD_Fused <- indici.errore(Tabella.SCAD_Fused)
# Salvo le varie metriche ottenute da tale modello 

# MCP ---------------------------------------------------------------------

lambda.grid.mcp <- exp(seq(-3.7,-4,l=100))

MCP_logistico_Fused <- cv.ncvreg(X_train,
                                 factor(train$Etichette_Fused_K2),
                                 family = "binomial",
                                 penalty = "MCP",
                                 nfolds = 5,
                                 fold = fold,
                                 trace = T,
                                 lambda = lambda.grid.mcp,
                                 seed = 42)
# Viene stimato un modello logistico con penalita SCAD, in CV a 5 fold con 
# metrica di ottimizzazione la verosimiglianza negativa binomiale,
# viene oltretutto fissato un seed per la riproducibilità dei risultati


n.coef_MCP_Fused <- MCP_logistico_Fused$min
# Numero di coefficienti diversi da 0

plot(MCP_logistico_Fused)
# Visualizzo andamento della verosimiglianza negativa per i vari lambda

MCP_logistico_Fused.fit <- ncvreg(X_train,
                                  factor(train$Etichette_Fused_K2),
                                  family = "binomial",
                                  penalty = "MCP",
                                  #lambda = lambda.grid, 
                                  trace=T)
plot(MCP_logistico_Fused.fit)
abline(v=MCP_logistico_Fused$lambda.min)
# Grafico 

MCP_logistico_Fused.fit$lambda[MCP_logistico_Fused.fit$convex.min]
MCP_logistico_Fused$lambda.min

y.hat.MCP_Fused <- predict(MCP_logistico_Fused, 
                           X_test, 
                           s = MCP_logistico_Fused$lambda.min, 
                           type = "class")
# Calcolo le previsioni del modello ottenendo direttamente come tipo la classe

Tabella.MCP_Fused <- tabella.sommario(test$Etichette_Fused_K2 ,
                                      y.hat.MCP_Fused)
Metriche_MCP_Fused <- indici.errore(Tabella.MCP_Fused)
# Salvo le varie metriche ottenute da tale modello 


# SVM - Elastic Fused ----------------------------------------------------------

lambda.grid.svm <- exp(seq(-4,-3,l=100))
# Si tiene una griglia per lambda ristretta cosi venga 
# esplorato un sottoinsieme di valori ragionevoli 

alpha.grid.svm <- c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
error.alpha.svm <- rep(NA, length(alpha.grid.svm))
se.alpha.svm <- rep(NA, length(alpha.grid.svm))
# Per far si che il modello SVM abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializzano
# due vettori che conterranno l'errore di classificazione medio e la relativa
# deviazione standard, nel caso presente nella griglia viene messo anche il 
# valore 1 relativo alla penalità lasso per valutare quale sia il valore di 
# alpha che ottimizza l'errore

for(i in 1:length(alpha.grid)){
  
  fit <- cv.sparseSVM(X_train,
                      ifelse(train$Etichette_Fused_K2==1,-1,1),
                      nfolds = 5,
                      fold.id = fold,
                      trace = T, 
                      alpha = alpha.grid[i],
                      seed = 42,
                      lambda = lambda.grid)
  # Viene stimato un modello SVM con penalita Elastic-Net,
  # in CV a 5 fold con metrica di ottimizzazione l'errore di errata
  # classificazione, per ogni valore di alpha contenuto in alpha.grid,
  # viene oltretutto fissato un seed per la riproducibilità dei risultati
  
  error.alpha[i] <- fit$cve[which.min(fit$cve)]
  se.alpha[i] <- fit$cvse[which.min(fit$cve)]
  # La procedura valuta per ogni modello di parametro alpha_i il miglior 
  # errore con standard deviation associata, per poi essere tra loro confrontati
  
}

error.alpha.svm[which.min(error.alpha.svm)]
# Minimo dell'errore di classificazione al variare dell'alpha

best.alpha_Fused_SVM <- alpha.grid.svm[which.min(error.alpha.svm)]
best.alpha_Fused_SVM
# Viene definito l'alpha per il quale si ottiene l'errore di classificazione 
# minore


stderrcv.k.svm.al <- se.alpha.svm
`MissClassificationError` <-  error.alpha.svm
`Alpha` <- alpha.grid.svm
data.plot.cv.svm.al <- tibble(`MissClassificationError`,`Alpha`)
# Vengono salvati i risultati ottenuti per i diversi alpha al fine di riprodurre
# un grafico per l'andamento dell'errore 

srs.acc_Fused_SVM <- ggplot(data.plot.cv.svm.al, 
                            mapping = aes(x = `Alpha`, 
                                          y = `MissClassificationError`)) +
  ylim(c(min(`MissClassificationError` - stderrcv.k.svm.al)-0.005,
         max(`MissClassificationError` + stderrcv.k.svm.al)+0.005))+
  geom_point(aes(x = `Alpha`, y = `MissClassificationError` + stderrcv.k.svm.al),
             shape = 95, size = 10) +
  geom_point(aes(x = `Alpha`, y = `MissClassificationError` - stderrcv.k.svm.al),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Alpha`, y = `MissClassificationError` - stderrcv.k.svm.al,
                   xend = `Alpha`, 
                   yend = `MissClassificationError` + stderrcv.k.svm.al)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("MissClassificationError CV - SVM ")
srs.acc_Fused_SVM
# Visualizzazione dell'errore di classificazione per i diversi alpha 

E.Net_SVM_Fused <- cv.sparseSVM(X_train,
                                ifelse(train$Etichette_Fused_K2==1,-1,1),
                                nfolds = 5,
                                fold.id = fold,
                                alpha = best.alpha_Fused_SVM,
                                trace = T,
                                lambda = lambda.grid,
                                seed = 42)
# Viene ristimato il modello logistico con penalita Elastic-Net fissata dal 
# best.alpha_Fused, in CV a 5 fold con metrica di ottimizzazione l'errore di 
# errata classificazione, per ogni valore di alpha contenuto in alpha.grid,
# viene oltretutto fissato un seed per la riproducibilità dei risultati

plot(E.Net_SVM_Fused)
# Visualizzo l'andamento dell'errore di classificazione per i diversi lambda
# del modello SVM Elastic-Net

n.coef_E.Net_SVM_Fused <- length(which(coef(E.Net_SVM_Fused, lambda=E.Net_SVM_Fused$lambda.min)!=0))
# Numero di coeffienti diversi da 0

y.hat.E.Net.SVM_Fused <- predict(E.Net_SVM_Fused,
                                 X_test,
                                 lambda = E.Net_SVM_Fused$lambda.min,
                                 type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

y.hat.E.Net.SVM_Fused <- ifelse(y.hat.E.Net.SVM_Fused == -1,1,2)
# Per migliorare le prestazioni del modello, questo viene stimato cambiando
# i livelli della variabile risposta in -1, 1 per tanto una volta stimato,
# per avere metriche coerenti si riportano i livelli di partenza 

Tabella.E.Net_Fused_SVM <- tabella.sommario(test$Etichette_Fused_K2 , 
                                            y.hat.E.Net.SVM_Fused)
Metriche_E.Net_SVM_Fused <- indici.errore(Tabella.E.Net_Fused_SVM)
# Salvo le varie metriche ottenute da tale modello 


# NSC ---------------------------------------------------------------------

result_Fused <- list(x = t(scale(X_train)),
                     y = factor(train$Etichette_Fused_K2))
# Definisco una lista contenente la matrice del disegno e la variabile risposta

nsc_Fused <- pamr.train(result_Fused)
# Stimo il modello

best.t_Fused <- nsc_Fused$threshold[which.min(nsc_Fused$errors)]
# Delimito la miglior soglia (parametro di regolarizzazione) in base all'errore
# ottenuto sul train

nsc.opt_Fused <- pamr.train(result_Fused, threshold = best.t_Fused)
# Ristimo il modello per la soglia ottimale

y.hat.nsc_Fused <- pamr.predict(nsc.opt_Fused, 
                                t(scale(X_test)), 
                                threshold = best.t_Fused)
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

scen_Fused <- pamr.predict(nsc.opt_Fused, 
                           result_Fused$x, 
                           threshold = best.t_Fused, 
                           type="cent")
# Calcolo i centroidi ovvero i valori delle variabili diverse da zero tenute
# dal modello al fine di poterle visualizzare 

dif_Fused <- (scen_Fused - nsc.opt_Fused$centroid.overall)/(nsc.opt_Fused$sd)
# Poiche i valori delle covariate vengono passate standardizzate, per effetuare
# un confronto sensato si standardizzano anche i valori dei centroidi 


par(mfrow = c(1,2))
plot(y = 1:length(nsc.opt_Fused$centroids[,1]), 
     x = nsc.opt_Fused$centroids[,1], 
     col="lightgrey", 
     type="l",
     main = "Fused label 1",
     xlab = "Centroidi",
     ylab = "Pixel")
abline(v=0, col="slateblue")
lines(y = 1:length(dif_Fused[,1]),
      x = dif_Fused[,1], 
      col="blue", 
      lwd=3)

plot(y = 1:length(nsc.opt_Fused$centroids[,2]), 
     x = nsc.opt_Fused$centroids[,2], 
     col="lightgrey", 
     type="l",
     main = "Fused label 2",
     xlab = "Centroidi",
     ylab = "Pixel")
abline(v=0, col="slateblue")
lines(y = 1:length(dif_Fused[,2]),
      x = dif_Fused[,2], 
      col="blue", 
      lwd=3)
par(mfrow = c(1,1))
# Visualizzo il comportamento medio dei pixel in base ai centrodi stimati


n.coef_NSC_Fused <- length(which(dif_Fused[,1]!=0))
# Numero di coefficienti diversi da 0
length(which(dif_Fused[,1]!=0))
length(which(dif_Fused[,2]!=0))
# risulta uguale anche per la classe 2

which(dif_Fused[,1]!=0)==which(dif_Fused[,1]!=0)
which(dif_Fused[,1]!=0)==which(dif_Fused[,2]!=0)
# Pixel diversi da 0: risultano uguali per entrambe le classi

Tabella.nsc_Fused <- tabella.sommario(y.hat.nsc_Fused, test$Etichette_Fused_K2)
Metriche_NSC_Fused <- indici.errore(Tabella.nsc_Fused)
# Salvo le varie metriche ottenute da tale modello 





# _____________ -----------------------------------------------------------
# Confronti Finali --------------------------------------------------------


Metriche_tot_Fused = sapply(grep("Metriche", ls(), value = T), get)
colnames(Metriche_tot_Fused) <- gsub("Metriche_","",colnames(Metriche_tot_Fused))
# Salvo tutte le metriche di tutti i modelli in un unico oggetto al
# fine di poter confrontare meglio tutti i modelli 
knitr::kable(Metriche_tot_Fused)


Tab_tot_Fused = sapply(grep("Tabella", ls(), value = T), get)
colnames(Tab_tot_Fused) <- gsub("Tabella_","",colnames(Tab_tot_Fused))
kappa.finali <- c()
for(i in 1:6){
  kappa.finali <- c(kappa.finali,Kappa(matrix(Tab_tot_Fused[,i], 2,2, byrow=F))$Unweighted[1])
}
names(kappa.finali) = gsub("Tabella_","",colnames(Tab_tot_Fused))
kappa.finali <- data.frame(kappa.finali)
names(kappa.finali) <- "Kappa di Cohen"
knitr::kable(kappa.finali)
Tab_Kappa_Fused <- kappa.finali
# Calcolo il Kappa di Cohen per ogni modello


Tab_ncoef.Fused <- sapply(grep("n.coef", ls(), value = T), get)
names(Tab_ncoef.Fused) <- gsub("Metriche_","",colnames(Metriche_tot_Fused))
Tab_ncoef.Fused <- as.data.frame(Tab_ncoef.Fused)
colnames(Tab_ncoef.Fused) <- c("Numero di coefficienti diversi da 0")
knitr::kable(Tab_ncoef.Fused)
# Tabella con il numero di coefficienti tenuti dai modelli

tab.tot.fused <- rbind(Metriche_tot_Fused, Tab_ncoef.Fused$`Numero di coefficienti diversi da 0`)
row.names(tab.tot.fused)[10] <- "Coefficienti diversi da 0"
knitr::kable(tab.tot.fused)
# Tabella con tutte le metriche

# Salvo risultati ---------------------------------------------------------

rm(train)
rm(test)
rm(X_train)
rm(X_test)

# Elimino dall'ambiente gli insiemi di verifica e stima utilizzati

base::save.image("Modelli_Fused_def1.RData")

rm(result_Fused)

base::save.image("Modelli_Fused_def1_RMD.RData")
