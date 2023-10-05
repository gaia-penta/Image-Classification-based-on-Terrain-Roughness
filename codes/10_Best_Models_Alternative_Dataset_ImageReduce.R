rm(list=ls())
load("Modelli_RID_def1.RData")
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
library(vcd)

# Carico dati --------------------------------------------------------

load("Modelli_RID_def.RData")
load("ENET_Kmeans_rid.RData")

dati <- fread("Dataset/Pixel_piccoli.csv")
dati$V1 <- NULL
# Carico la matrice dei pixel ai quali C( stato applicato flsa

dati <- data.frame(fread("Dataset/Data_k2_k3_models.csv")[,c(1:10)], dati)
# Prendo le etichette da modellare

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
  SensibilitC <- round((tabella[4]/sum(tabella[3],tabella[4])),3)
  SpecificitC <- round((tabella[1]/sum(tabella[1],tabella[2])),3)
  alpha <- round((tabella[2]/sum(tabella[1],tabella[2])),3)
  beta <- round((tabella[3]/sum(tabella[3],tabella[4])),3)
  False.Disc.Rate<- round(alpha/(1+alpha-beta),3)
  Precisione <-round((1-beta)/(1+alpha-beta),3)
  F1.Score<- round(2*tabella[4]/(2*tabella[4]+tabella[2]+tabella[3]),3)
  
  False.positive.rate <- alpha
  False.negative.rate <- beta
  
  indici<-data.frame(Errata.Classificazione,
                     Accuratezza,
                     SensibilitC ,
                     SpecificitC ,
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
# PoichC( la maggior parte delle funzioni per modellare richiede che la matrice
# del disegno sia un oggetto matrice converto gli insiemi di stima e verifica 
# in matrici
# Elastic net Logistico -------------------------------------------------------

lambda.grid <- exp(seq(-5,-2.5,l=100))
# Come in precedenza si tiene una griglia per lambda ristretta cosi venga 
# esplorato un sottoinsieme di valori ragionevoli 

alpha.grid <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
error.alpha <- rep(NA, length(alpha.grid))
se.alpha <- rep(NA, length(alpha.grid))
# Per far si che il modello abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializzano
# due vettori che conterranno l'errore di classificazione medio e la relativa
# deviazione standard


for(i in 1:length(alpha.grid)){
  
  registerDoMC(cores = 5)
  # Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
  # che intendo utilizzare per parallelizzare il modello
  
  fit <- cv.glmnet(X_train,
                   factor(train$Etichette_Fused_K2),
                   alpha = alpha.grid[i],
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
  # viene oltretutto fissato un seed per la riproducibilitC  dei risultati
  
  error.alpha[i] <- fit$cvm[fit$index["min",]]
  se.alpha[i] <- fit$cvsd[fit$index["min",]]
  # La procedura valuta per ogni modello di parametro alpha_i il miglior 
  # errore con standard deviation associata, per poi essere tra loro confrontati
  
}

stderrcv.k <- se.alpha
`MissClassificationError` <-  error.alpha
`Alpha` <- alpha.grid
data.plot.cv <- tibble(`MissClassificationError`,`Alpha`)
# Vengono salvati i risultati ottenuti per i diversi alpha al fine di riprodurre
# un grafico per l'andamento dell'errore 

srs.acc_Fused <- ggplot(data.plot.cv, 
                        mapping = aes(x = `Alpha`, 
                                      y = `MissClassificationError`)) +
  ylim(c(min(`MissClassificationError` - stderrcv.k)-0.005,
         max(`MissClassificationError` + stderrcv.k)+0.005))+
  geom_point(aes(x = `Alpha`, y = `MissClassificationError` + stderrcv.k),
             shape = 95, size = 10) +
  geom_point(aes(x = `Alpha`, y = `MissClassificationError` - stderrcv.k),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Alpha`, y = `MissClassificationError` - stderrcv.k,
                   xend = `Alpha`, 
                   yend = `MissClassificationError` + stderrcv.k)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("MissClassificationError  CV")
srs.acc_Fused
# Visualizzazione dell'errore di classificazione per i diversi alpha 

error.alpha[which.min(error.alpha)]
# Minimo dell'errore di classificazione al variare dell'alpha

best.alpha_Fused <- alpha.grid[which.min(error.alpha)]
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
# viene oltretutto fissato un seed per la riproducibilitC  dei risultati

plot(E.Net_logistico_Fused)
# Visualizzo l'andamento dell'errore di classificazione per i diversi lambda
# del modello Elastic-Net

n.coef_Enet_Fused <- length(which(coef(E.Net_logistico_Fused, s=E.Net_logistico_Fused$lambda.min)!=0))
# Numero di coefficienti diverso da 0


y.hat.E.Net_Fused <- predict(E.Net_logistico_Fused, 
                             s = E.Net_logistico_Fused$lambda.min, 
                             newx = X_test, 
                             type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.E.Net_Fused <- tabella.sommario(test$Etichette_Fused_K2 , 
                                        y.hat.E.Net_Fused)
Metriche_E.Net_Fused <- indici.errore(Tabella.E.Net_Fused)
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


n.coef_NSC_Fused_rid <- length(which(dif_Fused[,1]!=0))
length(which(dif_Fused[,2]!=0))



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

Tabella.nsc_Fused <- tabella.sommario(test$Etichette_Fused_K2 , 
                                      y.hat.nsc_Fused)
Metriche_NSC_Fused <- indici.errore(Tabella.nsc_Fused)
# Salvo le varie metriche ottenute da tale modello 



# Elastic net Logistico -------------------------------------------------------

lambda.grid <- exp(seq(-7,-4,l=100))
# Come in precedenza si tiene una griglia per lambda ristretta cosi venga 
# esplorato un sottoinsieme di valori ragionevoli 

alpha.grid <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
error.alpha <- rep(NA, length(alpha.grid))
se.alpha <- rep(NA, length(alpha.grid))
# Per far si che il modello abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializzano
# due vettori che conterranno l'errore di classificazione medio e la relativa
# deviazione standard

for(alpha in 1:length(alpha.grid)){
  
  registerDoMC(cores = 5)
  # Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
  # che intendo utilizzare per parallelizzare il modello
  
  fit <- cv.glmnet(X_train,
                   factor(train$Etichette_Kmeans_K2),
                   alpha = alpha.grid[alpha],
                   type.measure = "class",
                   family = "binomial",
                   nfolds = 5,
                   foldid = fold,
                   lambda = lambda.grid,
                   parallel = T,
                   trace.it = 1,
                   seed = 42,
                   keep = T)
  # Viene stimato un modello logistico con penalita Elastic-Net, in CV a 5 fold
  # con metrica di ottimizzazione l'errore di errata classificazione,
  # viene oltretutto fissato un seed per la riproducibilitC  dei risultati.
  # Con l'etichetta presente perC2 occorre fare una considerazione, ci troviamo 
  # in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
  # metrica come l'errore di errata classificazione potrebbe portare a 
  # conclusioni inestatte, per tanto viene impostato un keep = T all'interno del
  # modello, che permette di avere per ogni lambda la stima del predittore 
  # lineare delle unita statistiche che stanno nell'out of fold, cosi da
  # riprodurre una CV fatta a mano utilizzando i risultati ottenuti dalla 
  # funzione cv.glment
  
  eta <- fit$fit.preval
  pi.train <- exp(eta)/(1+exp(eta))
  pred.train.class <- ifelse(pi.train > 1/2, 2, 1)
  # Mi riconduco dal predittore linare alle classi stimate 
  
  kappa.vals <- matrix(NA, ncol(pred.train.class), 5)
  # Una metrica ragionevole per il caso presente di sbilanciamento C( il Kappa di
  # cholen, per tanto inizializzo una matrice vuota con numero di righe pari al 
  # numero di lambda, e numero di colonne pari ai fold utilizzati
  
  for( i in 1:ncol(pred.train.class)){
    
    for(j in 1:5){
      ind.fold.out <- which(fit$foldid == j)
      # Definisco il fold corrente
      
      if (length(unique(pred.train.class[ind.fold.out,i])) != 1)
        # Condizione per la quale si valuta la metrica relativa a quel fold, di
        # un lambda se e solo se le classi stimate non hanno solo un livello
      {
        
        tab <- table(pred.train.class[ind.fold.out,i], 
                     train$Etichette_Kmeans_K2[ind.fold.out])
        kappa.vals[i,j] <- Kappa(tab)$Unweighted[1]
        # Calcolo Kappa e salvo il risultato per il j-esimo fold
        
      }
    }
  }
  
  error.Lambda <- apply(kappa.vals,1,mean)
  se.Lambda <- apply(kappa.vals,1,sd)
  # Calcolo Kappa medio per ogni fold e relativa deviazione standard
  
  error.alpha[alpha] <- error.Lambda[which.max(error.Lambda)]
  se.alpha[alpha] <- se.Lambda[which.max(error.Lambda)]
  # La procedura valuta per ogni modello di parametro alpha_i il miglior 
  # errore con standard deviation associata, per poi essere tra loro confrontati
  
}

stderrcv.k <- se.alpha
`Kappa Cohen` <-  error.alpha
`Alpha` <- alpha.grid
data.plot.cv <- tibble(`Kappa Cohen`,`Alpha`)
# Vengono salvati i risultati ottenuti per i diversi alpha al fine di riprodurre
# un grafico per l'andamento dell'errore 

srs.acc_Kmeans.alpha <- ggplot(data.plot.cv, 
                               mapping = aes(x = `Alpha`, 
                                             y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k)-0.005,
         max(`Kappa Cohen` + stderrcv.k)+0.005))+
  geom_point(aes(x = `Alpha`, y = `Kappa Cohen` + stderrcv.k),
             shape = 95, size = 10) +
  geom_point(aes(x = `Alpha`, y = `Kappa Cohen` - stderrcv.k),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Alpha`, y = `Kappa Cohen` - stderrcv.k,
                   xend = `Alpha`, 
                   yend = `Kappa Cohen` + stderrcv.k)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen  CV")
srs.acc_Kmeans.alpha
# Visualizzazione di Kappa per i diversi alpha 

error.alpha[which.max(error.alpha)]
# Massimo di Kappa al variare dell'alpha

best.alpha_Kmeans <- alpha.grid[which.max(error.alpha)]
best.alpha_Kmeans
# Viene definito l'alpha per il quale si ottiene il Kappa maggiore

E.Net_logistico_Kmeans <- cv.glmnet(X_train,
                                    factor(train$Etichette_Kmeans_K2),
                                    type.measure = "class",
                                    family = "binomial",
                                    alpha = best.alpha_Kmeans,
                                    nfolds = 5,
                                    foldid = fold,
                                    lambda = lambda.grid,
                                    parallel = T,
                                    trace.it = 1,
                                    seed = 42,
                                    keep = T)
# Viene ristimato il modello logistico con penalita Elastic-Net fissata dal 
# best.alpha_Kmeans

plot(E.Net_logistico_Kmeans)
# Visualizzo l'andamento dell'errore di classificazione per i diversi lambda
# del modello Elastic-Net

eta <- E.Net_logistico_Kmeans$fit.preval
pi.train <- exp(eta)/(1+exp(eta))
pred.train.class <- ifelse(pi.train > 1/2, 2, 1)
# Mi riconduco dal predittore linare alle classi stimate 

kappa.vals <- matrix(NA, ncol(pred.train.class), 5)
# Inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for(i in 1:ncol(pred.train.class)){
  
  for(j in 1:5){
    ind.fold.out <- which(E.Net_logistico_Kmeans$foldid == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class[ind.fold.out,i], 
                   train$Etichette_Kmeans_K2[ind.fold.out])
      kappa.vals[i,j] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per il j-esimo fold
      
    }
  }
}

error.Lambda.kmeans <- apply(kappa.vals,1,mean)
se.Lambda.kmeans <- apply(kappa.vals,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k <- se.Lambda
`Kappa Cohen` <-  error.Lambda
`Log(Lambda)` <- log(E.Net_logistico_Kmeans$lambda)
data.plot.cv <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans.Enet <- ggplot(data.plot.cv, 
                              mapping = aes(x = `Log(Lambda)`, 
                                            y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k)-0.005,
         max(`Kappa Cohen` + stderrcv.k)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` + stderrcv.k),
             shape = 95, size = 10) +
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k,
                   xend = `Log(Lambda)`, 
                   yend = `Kappa Cohen` + stderrcv.k)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen  CV")
srs.acc_Kmeans.Enet
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 

lambda.k.kmeans <- E.Net_logistico_Kmeans$lambda[which.max(error.Lambda.kmeans)]
# Definisco il lambda che massimizza Kappa


n.coef_Enet_Kmeans <- length(which(coef(E.Net_logistico_Kmeans, s =  lambda.k.kmeans)!=0))
# Numero di coefficeinti diversi da 0

y.hat.E.Net_Kmeans <- predict(E.Net_logistico_Kmeans,
                              s = lambda.k.kmeans,
                              newx = X_test,
                              type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.E.Net_Kmeans <- tabella.sommario(y.hat.E.Net_Kmeans,test$Etichette_Kmeans_K2
                                         )
Metriche_E.Net_Kmeans <- indici.errore(Tabella.E.Net_Kmeans)
# Salvo le varie metriche ottenute da tale modello 




# Elastic net Logistico -------------------------------------------------------

lambda.grid <- exp(seq(-7,-4,l=100))
# Come in precedenza si tiene una griglia per lambda ristretta cosi venga 
# esplorato un sottoinsieme di valori ragionevoli 

alpha.grid <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
error.alpha <- rep(NA, length(alpha.grid))
se.alpha <- rep(NA, length(alpha.grid))
# Per far si che il modello abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializzano
# due vettori che conterranno l'errore di classificazione medio e la relativa
# deviazione standard

for(alpha in 1:length(alpha.grid)){
  
  registerDoMC(cores = 5)
  # Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
  # che intendo utilizzare per parallelizzare il modello
  
  fit <- cv.glmnet(X_train,
                   factor(train$Etichette_Kaggle_K2),
                   alpha = alpha.grid[alpha],
                   type.measure = "class",
                   family = "binomial",
                   nfolds = 5,
                   foldid = fold,
                   lambda = lambda.grid,
                   parallel = T,
                   trace.it = 1,
                   seed = 42,
                   keep = T)
  # Viene stimato un modello logistico con penalita Elastic-Net, in CV a 5 fold
  # con metrica di ottimizzazione l'errore di errata classificazione,
  # viene oltretutto fissato un seed per la riproducibilitC  dei risultati.
  # Con l'etichetta presente perC2 occorre fare una considerazione, ci troviamo 
  # in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
  # metrica come l'errore di errata classificazione potrebbe portare a 
  # conclusioni inestatte, per tanto viene impostato un keep = T all'interno del
  # modello, che permette di avere per ogni lambda la stima del predittore 
  # lineare delle unita statistiche che stanno nell'out of fold, cosi da
  # riprodurre una CV fatta a mano utilizzando i risultati ottenuti dalla 
  # funzione cv.glment
  
  eta <- fit$fit.preval
  pi.train <- exp(eta)/(1+exp(eta))
  pred.train.class <- ifelse(pi.train > 1/2, 0, 1)
  # Mi riconduco dal predittore linare alle classi stimate 
  
  kappa.vals <- matrix(NA, ncol(pred.train.class), 5)
  # Una metrica ragionevole per il caso presente di sbilanciamento C( il Kappa di
  # cholen, per tanto inizializzo una matrice vuota con numero di righe pari al 
  # numero di lambda, e numero di colonne pari ai fold utilizzati
  
  for( i in 1:ncol(pred.train.class)){
    
    for(j in 1:5){
      ind.fold.out <- which(fit$foldid == j)
      # Definisco il fold corrente
      
      if (length(unique(pred.train.class[ind.fold.out,i])) != 1)
        # Condizione per la quale si valuta la metrica relativa a quel fold, di
        # un lambda se e solo se le classi stimate non hanno solo un livello
      {
        
        tab <- table(pred.train.class[ind.fold.out,i], 
                     train$Etichette_Kaggle_K2[ind.fold.out])
        kappa.vals[i,j] <- Kappa(tab)$Unweighted[1]
        # Calcolo Kappa e salvo il risultato per il j-esimo fold
        
      }
    }
  }
  
  error.Lambda <- apply(kappa.vals,1,mean)
  se.Lambda <- apply(kappa.vals,1,sd)
  # Calcolo Kappa medio per ogni fold e relativa deviazione standard
  
  error.alpha[alpha] <- error.Lambda[which.max(error.Lambda)]
  se.alpha[alpha] <- se.Lambda[which.max(error.Lambda)]
  # La procedura valuta per ogni modello di parametro alpha_i il miglior 
  # errore con standard deviation associata, per poi essere tra loro confrontati
  
}

stderrcv.k <- se.alpha
`Kappa Cohen` <-  error.alpha
`Alpha` <- alpha.grid
data.plot.cv <- tibble(`Kappa Cohen`,`Alpha`)
# Vengono salvati i risultati ottenuti per i diversi alpha al fine di riprodurre
# un grafico per l'andamento dell'errore 

srs.acc_Kaggle.alpha <- ggplot(data.plot.cv, 
                               mapping = aes(x = `Alpha`, 
                                             y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k)-0.005,
         max(`Kappa Cohen` + stderrcv.k)+0.005))+
  geom_point(aes(x = `Alpha`, y = `Kappa Cohen` + stderrcv.k),
             shape = 95, size = 10) +
  geom_point(aes(x = `Alpha`, y = `Kappa Cohen` - stderrcv.k),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Alpha`, y = `Kappa Cohen` - stderrcv.k,
                   xend = `Alpha`, 
                   yend = `Kappa Cohen` + stderrcv.k)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen  CV")
srs.acc_Kaggle.alpha
# Visualizzazione di Kappa per i diversi alpha 

error.alpha[which.max(error.alpha)]
# Massimo di Kappa al variare dell'alpha

best.alpha_Kaggle <- alpha.grid[which.max(error.alpha)]
best.alpha_Kaggle
# Viene definito l'alpha per il quale si ottiene il Kappa maggiore

lambda.grid <- exp(seq(-5,-3,l=100))


E.Net_logistico_Kaggle <- cv.glmnet(X_train,
                                    factor(train$Etichette_Kaggle_K2),
                                    type.measure = "class",
                                    family = "binomial",
                                    alpha = best.alpha_Kaggle,
                                    nfolds = 5,
                                    foldid = fold,
                                    lambda = lambda.grid,
                                    parallel = T,
                                    trace.it = 1,
                                    seed = 42,
                                    keep = T)
# Viene ristimato il modello logistico con penalita Elastic-Net fissata dal 
# best.alpha_Kaggle

plot(E.Net_logistico_Kaggle)
# Visualizzo l'andamento dell'errore di classificazione per i diversi lambda
# del modello Elastic-Net

eta <- E.Net_logistico_Kaggle$fit.preval
pi.train <- exp(eta)/(1+exp(eta))
pred.train.class <- ifelse(pi.train > 1/2, 0, 1)
# Mi riconduco dal predittore linare alle classi stimate 

kappa.vals <- matrix(NA, ncol(pred.train.class), 5)
# Inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for( i in 1:ncol(pred.train.class)){
  
  for(j in 1:5){
    ind.fold.out <- which(E.Net_logistico_Kaggle$foldid == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class[ind.fold.out,i], 
                   train$Etichette_Kaggle_K2[ind.fold.out])
      kappa.vals[i,j] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per il j-esimo fold
      
    }
  }
}

error.Lambda <- apply(kappa.vals,1,mean)
se.Lambda <- apply(kappa.vals,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k <- se.Lambda
`Kappa Cohen` <-  error.Lambda
`Log(Lambda)` <- log(E.Net_logistico_Kaggle$lambda)
data.plot.cv <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kaggle.Enet <- ggplot(data.plot.cv, 
                              mapping = aes(x = `Log(Lambda)`, 
                                            y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k)-0.005,
         max(`Kappa Cohen` + stderrcv.k)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` + stderrcv.k),
             shape = 95, size = 10) +
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k,
                   xend = `Log(Lambda)`, 
                   yend = `Kappa Cohen` + stderrcv.k)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen  CV")
srs.acc_Kaggle.Enet
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 


lambda.k.kaggle <- E.Net_logistico_Kaggle$lambda[which.max(error.Lambda)]
# Definisco il lambda che massimizza Kappa


n.coef_Enet_Kaggle <- length(which(coef(E.Net_logistico_Kmeans, s =  lambda.k.kaggle)!=0))

y.hat.E.Net_Kaggle <- predict(E.Net_logistico_Kaggle,
                              s = lambda.k,
                              newx = X_test,
                              type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.E.Net_Kaggle <- tabella.sommario(test$Etichette_Kaggle_K2 ,
                                         y.hat.E.Net_Kaggle)
Metriche_E.Net_Kaggle <- indici.errore(Tabella.E.Net_Kaggle)
# Salvo le varie metriche ottenute da tale modello



# _____________ -----------------------------------------------------------
# Confronti Finali --------------------------------------------------------

Metriche_tot_RID = sapply(grep("Metriche", ls(), value = T), get)
colnames(Metriche_tot_RID) <- gsub("Metriche_","",colnames(Metriche_tot_RID))
# Salvo tutte le metriche di tutti i modelli in un unico oggetto al
# fine di poter confrontare meglio tutti i modelli 

knitr::kable(Metriche_tot_RID)
# Visualizzo le diverse metriche per i diversi modelli



Tab_tot_RID = sapply(grep("Tabella", ls(), value = T), get)
colnames(Tab_tot_RID) <- gsub("Tabella_","",colnames(Tab_tot_RID))
kappa.finali <- c()
for(i in 1:4){
  kappa.finali <- c(kappa.finali,Kappa(matrix(Tab_tot_RID[,i], 2,2, byrow=F))$Unweighted[1])
}
names(kappa.finali) = gsub("Tabella_","",colnames(Tab_tot_RID))
kappa.finali <- data.frame(kappa.finali)
names(kappa.finali) <- "Kappa di Cohen"
knitr::kable(kappa.finali)
Tab_Kappa_RID <- kappa.finali
# Tabella con valore di Kappa per ciascun modello

Tab_ncoef.RID <- sapply(grep("n.coef", ls(), value = T), get)
#names(Tab_ncoef.Kmeans) <- gsub("Metriche_","",colnames(Metriche_tot))
Tab_ncoef.RID <- as.data.frame(Tab_ncoef.RID)
colnames(Tab_ncoef.RID) <- c("Numero di coefficienti diversi da 0")
knitr::kable(Tab_ncoef.RID)
# Tabella con il numero di coefficienti tenuti dai modelli



# Salvo risultati ---------------------------------------------------------

rm(train)
rm(test)
rm(X_train)
rm(X_test)
# Elimino dall'ambiente gli insiemi di verifica e stima utilizzati

save.image("Modelli_RID_def1.RData")
save(Metriche_tot_RID, Tab_Kappa_RID, Tab_ncoef.RID, file="metriche_RID1.RData")


save.image("ENET_Kmeans_rid.RData")
