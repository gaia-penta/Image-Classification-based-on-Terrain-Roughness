rm(list = ls())
load("Modelli_Kmeans_def3.RData")

current.path <- getwd()
# Workdirectory "Progetto"

# Librerie utilizzate -----------------------------------------------------

library(data.table)
library(glmnet)
library(sparseSVM)
library(ncvreg)
library(tidyverse)
library(vcd)
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

# Lasso Logistico -------------------------------------------------------------

lambda.grid.lasso <- exp(seq(-7,-4,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia C( il risultato di prove precedenti che
# non vengono riportate

registerDoMC(cores = 20)
# Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
# che intendo utilizzare per parallelizzare il modello 

Lasso_logistico_Kmeans <- cv.glmnet(X_train,
                                    factor(train$Etichette_Kmeans_K2),
                                    type.measure = "class",
                                    family = "binomial",
                                    alpha = 1,
                                    nfolds = 5,
                                    foldid = fold,
                                    lambda = lambda.grid.lasso,
                                    parallel = T,
                                    trace.it = 1,
                                    seed = 42,
                                    keep = T)
# Viene stimato un modello logistico con penalita lasso, in CV a 5 fold con 
# metrica di ottimizzazione l'errore di errata classificazione,
# viene oltretutto fissato un seed per la riproducibilitC  dei risultati.
# Con l'etichetta presente perC2 occorre fare una considerazione, ci troviamo 
# in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
# metrica come l'errore di errata classificazione potrebbe portare a conclusioni
# inestatte, per tanto viene impostato un keep = T all'interno del modello, 
# che permette di avere per ogni lambda la stima del predittore lineare delle
# unita statistiche che stanno nell'out of fold, cosi da riprodurre una 
# CV fatta a mano utilizzando i risultati ottenuti dalla funzione cv.glment

plot(Lasso_logistico_Kmeans)
# Visualizzo errore di previsione, si nota la scarsa efficacia della metrica
length(which(coef(Lasso_logistico_Kmeans,s =  Lasso_logistico_Kmeans$lambda.min)!=0))
# [1] 69



# Grafico in ggplot dell'errore di classificazione

error.Lambda.lasso.miss <- rev(Lasso_logistico_Kmeans$cvm)
se.Lambda.lasso.miss <- rev(Lasso_logistico_Kmeans$cvsd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k.lasso.miss <- se.Lambda.lasso.miss
`MissClassificationError` <-  error.Lambda.lasso.miss
`Log(Lambda)` <- log(lambda.grid.lasso)
data.plot.cv.lasso.miss <- tibble(`MissClassificationError`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans.lasso.miss <- ggplot(data.plot.cv.lasso.miss, 
                                    mapping = aes(x = `Log(Lambda)`, 
                                                  y = `MissClassificationError`)) +
  ylim(c(min(`MissClassificationError` - stderrcv.k.lasso.miss)-0.005,
         max(`MissClassificationError` + stderrcv.k.lasso.miss)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, 
                 y = `MissClassificationError` + stderrcv.k.lasso.miss),
             shape = 95, size = 2, color="grey") +
  geom_point(aes(x = `Log(Lambda)`, 
                 y = `MissClassificationError` - stderrcv.k.lasso.miss),
             shape = 95, size = 2, color="grey") +
  geom_segment(aes(x = `Log(Lambda)`,
                   y = `MissClassificationError` - stderrcv.k.lasso.miss,
                   xend = `Log(Lambda)`, 
                   yend = `MissClassificationError` + stderrcv.k.lasso.miss), color="grey") +
  geom_point(col = "firebrick1", size = 1, shape=20) +
  ggtitle("Misclassification Error CV Lasso - K-means Labels")+ theme(legend.position = "bottom", 
                                                     axis.text = element_text(size = 8),
                                                     axis.title = element_text(size = 8),
                                                     legend.title=element_text(size=10), 
                                                     legend.text=element_text(size=9))
srs.acc_Kmeans.lasso.miss

eta.lasso <- Lasso_logistico_Kmeans$fit.preval
pi.train.lasso <- exp(eta.lasso)/(1+exp(eta.lasso))
pred.train.class.lasso <- ifelse(pi.train.lasso > 1/2, 2, 1)
# Mi riconduco dal predittore linare alle classi stimate 

kappa.vals.lasso <- matrix(NA, ncol(pred.train.class.lasso), 5)
# Una metrica ragionevole per il caso presente di sbilanciamento C( il Kappa di
# cholen, per tanto inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for( i in 1:ncol(pred.train.class.lasso)){
  
  for(j in 1:5){
    ind.fold.out <- which(Lasso_logistico_Kmeans$foldid == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class.lasso[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.lasso[ind.fold.out,i], 
                   train$Etichette_Kmeans_K2[ind.fold.out])
      kappa.vals.lasso[i,j] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per il j-esimo fold
      
    }
  }
}

error.Lambda.lasso <- apply(kappa.vals.lasso,1,mean)
se.Lambda.lasso <- apply(kappa.vals.lasso,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k.lasso <- se.Lambda.lasso
`Kappa Cohen` <-  error.Lambda.lasso
`Log(Lambda)` <- log(Lasso_logistico_Kmeans$lambda)
data.plot.cv.lasso <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 


srs.acc_Kmeans.lasso <- ggplot(data.plot.cv.lasso, 
                               mapping = aes(x = `Log(Lambda)`, 
                                             y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k.lasso)-0.005,
         max(`Kappa Cohen` + stderrcv.k.lasso)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` + stderrcv.k.lasso),
             shape = 95, size = 2, color="grey") +
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.lasso),
             shape = 95, size = 2, color="grey") +
  geom_segment(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.lasso,
                   xend = `Log(Lambda)`, 
                   yend = `Kappa Cohen` + stderrcv.k.lasso), color="grey") +
  geom_point(col = "firebrick1", size = 1, shape=20) +
  ggtitle("Kappa Cohen CV Lasso - K-means labels")+ theme(legend.position = "bottom", 
                                         axis.text = element_text(size = 8),
                                         axis.title = element_text(size = 8),
                                         legend.title=element_text(size=10), 
                                         legend.text=element_text(size=9))
srs.acc_Kmeans.lasso
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 

lambda.k.lasso <- Lasso_logistico_Kmeans$lambda[which.max(error.Lambda.lasso)]
# Definisco il lambda che massimizza Kappa


n.coef_Lasso_Kmeans <- length(which(coef(Lasso_logistico_Kmeans,s =  lambda.k.lasso)!=0))


y.hat.lasso_Kmeans <- predict(Lasso_logistico_Kmeans, 
                              s =  lambda.k.lasso, 
                              newx = X_test, 
                              type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.lasso_Kmeans  <- tabella.sommario(y.hat.lasso_Kmeans,
                                          test$Etichette_Kmeans_K2)
Metriche_Lasso_Kmeans <- indici.errore(Tabella.lasso_Kmeans)
# Salvo le varie metriche ottenute da tale modello 


# Elastic net Logistico -------------------------------------------------------

lambda.grid.enet <- exp(seq(-7,-4,l=100))
# Come in precedenza si tiene una griglia per lambda ristretta cosi venga 
# esplorato un sottoinsieme di valori ragionevoli 

alpha.grid.enet <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
error.alpha.enet <- rep(NA, length(alpha.grid.enet))
se.alpha.enet <- rep(NA, length(alpha.grid.enet))
# Per far si che il modello abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializzano
# due vettori che conterranno l'errore di classificazione medio e la relativa
# deviazione standard

for(alpha in 1:length(alpha.grid.enet)){
  
  registerDoMC(cores = 20)
  # Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
  # che intendo utilizzare per parallelizzare il modello
  
  fit <- cv.glmnet(X_train,
                   factor(train$Etichette_Kmeans_K2),
                   alpha = alpha.grid.enet[alpha],
                   type.measure = "class",
                   family = "binomial",
                   nfolds = 5,
                   foldid = fold,
                   lambda = lambda.grid.enet,
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
  
  eta.enet <- fit$fit.preval
  pi.train.enet <- exp(eta.enet)/(1+exp(eta.enet))
  pred.train.class.enet <- ifelse(pi.train.enet > 1/2, 2, 1)
  # Mi riconduco dal predittore linare alle classi stimate 
  
  kappa.vals.enet <- matrix(NA, ncol(pred.train.class.enet), 5)
  # Una metrica ragionevole per il caso presente di sbilanciamento C( il Kappa di
  # cholen, per tanto inizializzo una matrice vuota con numero di righe pari al 
  # numero di lambda, e numero di colonne pari ai fold utilizzati
  
  for( i in 1:ncol(pred.train.class.enet)){
    
    for(j in 1:5){
      ind.fold.out <- which(fit$foldid == j)
      # Definisco il fold corrente
      
      if (length(unique(pred.train.class.enet[ind.fold.out,i])) != 1)
        # Condizione per la quale si valuta la metrica relativa a quel fold, di
        # un lambda se e solo se le classi stimate non hanno solo un livello
      {
        
        tab <- table(pred.train.class.enet[ind.fold.out,i], 
                     train$Etichette_Kmeans_K2[ind.fold.out])
        kappa.vals.enet[i,j] <- Kappa(tab)$Unweighted[1]
        # Calcolo Kappa e salvo il risultato per il j-esimo fold
        
      }
    }
  }
  
  error.Lambda.enet <- apply(kappa.vals.enet,1,mean)
  se.Lambda.enet <- apply(kappa.vals.enet,1,sd)
  # Calcolo Kappa medio per ogni fold e relativa deviazione standard
  
  error.alpha.enet[alpha] <- error.Lambda.enet[which.max(error.Lambda.enet)]
  se.alpha.enet[alpha] <- se.Lambda.enet[which.max(error.Lambda.enet)]
  # La procedura valuta per ogni modello di parametro alpha_i il miglior 
  # errore con standard deviation associata, per poi essere tra loro confrontati
  
}

stderrcv.k.enet.alpha <- se.alpha.enet
`Kappa Cohen` <-  error.alpha.enet
`Alpha` <- alpha.grid.enet
data.plot.cv.enet.alpha <- tibble(`Kappa Cohen`,`Alpha`)
# Vengono salvati i risultati ottenuti per i diversi alpha al fine di riprodurre
# un grafico per l'andamento dell'errore 

srs.acc_Kmeans.alpha.enet <- ggplot(data.plot.cv.enet.alpha, 
                               mapping = aes(x = `Alpha`, 
                                             y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k.enet.alpha)-0.005,
         max(`Kappa Cohen` + stderrcv.k.enet.alpha)+0.005))+
  geom_point(aes(x = `Alpha`, y = `Kappa Cohen` + stderrcv.k.enet.alpha),
             shape = 95, size = 10) +
  geom_point(aes(x = `Alpha`, y = `Kappa Cohen` - stderrcv.k.enet.alpha),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Alpha`, y = `Kappa Cohen` - stderrcv.k.enet.alpha,
                   xend = `Alpha`, 
                   yend = `Kappa Cohen` + stderrcv.k.enet.alpha)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen CV Elastic Net")
srs.acc_Kmeans.alpha.enet
# Visualizzazione di Kappa per i diversi alpha 

error.alpha.enet[which.max(error.alpha.enet)]
# Massimo di Kappa al variare dell'alpha

best.alpha_Kmeans.enet <- alpha.grid.enet[which.max(error.alpha.enet)]
best.alpha_Kmeans.enet
# Viene definito l'alpha per il quale si ottiene il Kappa maggiore

E.Net_logistico_Kmeans <- cv.glmnet(X_train,
                                    factor(train$Etichette_Kmeans_K2),
                                    type.measure = "class",
                                    family = "binomial",
                                    alpha = best.alpha_Kmeans.enet,
                                    nfolds = 5,
                                    foldid = fold,
                                    lambda = lambda.grid.enet,
                                    parallel = T,
                                    trace.it = 1,
                                    seed = 42,
                                    keep = T)
# Viene ristimato il modello logistico con penalita Elastic-Net fissata dal 
# best.alpha_Kmeans

plot(E.Net_logistico_Kmeans)
# Visualizzo l'andamento dell'errore di classificazione per i diversi lambda
# del modello Elastic-Net

eta.enet.fit <- E.Net_logistico_Kmeans$fit.preval
pi.train.enet.fit <- exp(eta.enet.fit)/(1+exp(eta.enet.fit))
pred.train.class.enet.fit <- ifelse(pi.train.enet.fit > 1/2, 2, 1)
# Mi riconduco dal predittore linare alle classi stimate 

kappa.vals.enet.fit <- matrix(NA, ncol(pred.train.class.enet.fit), 5)
# Inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for( i in 1:ncol(pred.train.class.enet.fit)){
  
  for(j in 1:5){
    ind.fold.out <- which(E.Net_logistico_Kmeans$foldid == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class.enet.fit[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.enet.fit[ind.fold.out,i], 
                   train$Etichette_Kmeans_K2[ind.fold.out])
      kappa.vals.enet.fit[i,j] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per il j-esimo fold
      
    }
  }
}

error.Lambda.enet <- apply(kappa.vals.enet.fit,1,mean)
se.Lambda.enet <- apply(kappa.vals.enet.fit,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k.enet.lambda <- se.Lambda.enet
`Kappa Cohen` <-  error.Lambda.enet
`Log(Lambda)` <- log(E.Net_logistico_Kmeans$lambda)
data.plot.cv.enet.lambda <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans.Enet.Lambda <- ggplot(data.plot.cv.enet.lambda, 
                              mapping = aes(x = `Log(Lambda)`, 
                                            y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k.enet.lambda)-0.005,
         max(`Kappa Cohen` + stderrcv.k.enet.lambda)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` + stderrcv.k.enet.lambda),
             shape = 95, size = 10) +
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.enet.lambda),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.enet.lambda,
                   xend = `Log(Lambda)`, 
                   yend = `Kappa Cohen` + stderrcv.k.enet.lambda)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen CV Elastic Net - Lambda")
srs.acc_Kmeans.Enet.Lambda
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 

lambda.k.enet <- E.Net_logistico_Kmeans$lambda[which.max(error.Lambda.enet)]
# Definisco il lambda che massimizza Kappa

n.coef_ENet_Kmeans <- length(which(coef(E.Net_logistico_Kmeans,s =  lambda.k.enet)!=0))
# Numero di coefficienti diversi da 0

y.hat.E.Net_Kmeans <- predict(E.Net_logistico_Kmeans,
                              s = lambda.k.enet,
                              newx = X_test,
                              type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.E.Net_Kmeans <- tabella.sommario(y.hat.E.Net_Kmeans,test$Etichette_Kmeans_K2)
Metriche_E.Net_Kmeans <- indici.errore(Tabella.E.Net_Kmeans)
# Salvo le varie metriche ottenute da tale modello 



# SCAD --------------------------------------------------------------------

lambda.grid.scad <- exp(seq(-3,-7,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia C( il risultato di prove precedenti che
# non vengono riportate

SCAD_logistico_Kmeans <- cv.ncvreg(X_train,
                                   factor(train$Etichette_Kmeans_K2),
                                   family = "binomial",
                                   penalty = "SCAD",
                                   nfolds = 5,
                                   fold = fold,
                                   trace = T,
                                   lambda = lambda.grid.scad,
                                   gamma = 20,
                                   seed = 42,
                                   returnY = T)
# Viene stimato un modello logistico con penalita SCAD, in CV a 5 fold con 
# metrica di ottimizzazione la verosimiglianza negativa binomiale,
# viene oltretutto fissato un seed per la riproducibilitC  dei risultati.
# Con l'etichetta presente perC2 occorre fare una considerazione, ci troviamo 
# in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
# metrica come l'errore di errata classificazione potrebbe portare a conclusioni
# inestatte, per tanto viene impostato un returnY = T all'interno del modello, 
# che permette di avere per ogni lambda la stima del predittore lineare delle
# unita statistiche che stanno nell'out of fold, cosi da riprodurre una 
# CV fatta a mano utilizzando i risultati ottenuti dalla funzione cv.ncvreg

plot(SCAD_logistico_Kmeans)
# Visualizzo andamento della verosimiglianza negativa per i vari lambda

eta.scad <- SCAD_logistico_Kmeans$Y
pi.train.scad <- exp(eta.scad)/(1+exp(eta.scad))
pred.train.class.scad <- ifelse(pi.train.scad > 0.6, 2, 1)
# Mi riconduco dal predittore linare alle classi stimate, nel caso presente
# utilizzo come soglia di classificazione la proporzione campionaria delle 
# classi nell'insime di stima

kappa.vals.scad <- matrix(NA, ncol(pred.train.class.scad), 5)
# Inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for( i in 1:ncol(pred.train.class.scad)){
  
  for(j in 1:5){
    ind.fold.out <- which(SCAD_logistico_Kmeans$fold == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class.scad[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.scad[ind.fold.out,i], 
                   train$Etichette_Kmeans_K2[ind.fold.out])
      kappa.vals.scad[i,j] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per il j-esimo fold
      
    }
  }
}

error.Lambda.scad <- apply(kappa.vals.scad,1,mean)
se.Lambda.scad <- apply(kappa.vals.scad,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k.scad <- se.Lambda.scad
`Kappa Cohen` <-  error.Lambda.scad
`Log(Lambda)` <- log(SCAD_logistico_Kmeans$lambda)
data.plot.cv.scad <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans.SCAD <- ggplot(data.plot.cv.scad, 
                              mapping = aes(x = `Log(Lambda)`, 
                                            y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k.scad)-0.005,
         max(`Kappa Cohen` + stderrcv.k.scad)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` + stderrcv.k.scad),
             shape = 95, size = 10) +
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.scad),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.scad,
                   xend = `Log(Lambda)`, 
                   yend = `Kappa Cohen` + stderrcv.k.scad)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen CV - SCAD")
srs.acc_Kmeans.SCAD
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 

lambda.k.scad <- SCAD_logistico_Kmeans$lambda[which.max(error.Lambda.scad)]
# Definisco il lambda che massimizza Kappa

n.coef_SCAD_Kmeans <- length(which(coef(SCAD_logistico_Kmeans, s =  lambda.k.scad)!=0))
# Numero di coefficienti diversi da 0

SCAD_logistico_Kmeans.fit <- ncvreg(X_train,
                                   factor(train$Etichette_Kmeans_K2),
                                   family = "binomial",
                                   penalty = "SCAD",
                                   trace = T,
                                   gamma = 20)

plot(SCAD_logistico_Kmeans.fit)
abline(v=lambda.k.scad)
# Grafico dei beta selezionati al variare di lambda

SCAD_logistico_Kmeans.fit$lambda[SCAD_logistico_Kmeans.fit$convex.min]
# Lambda minimo tenuto prima di raggiungere la regione di non convessit??

y.hat.SCAD_Kmeans <- predict(SCAD_logistico_Kmeans, 
                             X_test, 
                             s = lambda.k.scad, 
                             type = "response")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la 
# probabilitC  delle classi 

y.hat.SCAD_Kmeans <- ifelse(y.hat.SCAD_Kmeans > 0.6, 2, 1)
# Utilizzo come soglia la proporzione campionaria dei livelli nell'insieme di
# stima

Tabella.SCAD_Kmeans <- tabella.sommario(y.hat.SCAD_Kmeans, test$Etichette_Kmeans_K2)
Metriche_SCAD_Kmeans <- indici.errore(Tabella.SCAD_Kmeans)
# Salvo le varie metriche ottenute da tale modello 


# MCP ---------------------------------------------------------------------

lambda.grid.mcp <- exp(seq(-3,-7,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia C( il risultato di prove precedenti che
# non vengono riportate

MCP_logistico_Kmeans <- cv.ncvreg(X_train,
                                  factor(train$Etichette_Kmeans_K2),
                                  family = "binomial",
                                  penalty = "MCP",
                                  nfolds = 5,
                                  fold = fold,
                                  trace = T,
                                  lambda = lambda.grid.mcp,
                                  gamma = 20,
                                  seed = 42,
                                  returnY = T)
# Viene stimato un modello logistico con penalita MCP, in CV a 5 fold con 
# metrica di ottimizzazione la verosimiglianza negativa binomiale,
# viene oltretutto fissato un seed per la riproducibilitC  dei risultati.
# Con l'etichetta presente perC2 occorre fare una considerazione, ci troviamo 
# in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
# metrica come l'errore di errata classificazione potrebbe portare a conclusioni
# inestatte, per tanto viene impostato un returnY = T all'interno del modello, 
# che permette di avere per ogni lambda la stima del predittore lineare delle
# unita statistiche che stanno nell'out of fold, cosi da riprodurre una 
# CV fatta a mano utilizzando i risultati ottenuti dalla funzione cv.ncvreg

plot(MCP_logistico_Kmeans)
  # Visualizzo andamento della verosimiglianza negativa per i vari lambda

eta.mcp <- MCP_logistico_Kmeans$Y
pi.train.mcp <- exp(eta.mcp)/(1+exp(eta.mcp))
pred.train.class.mcp <- ifelse(pi.train.mcp > 0.6, 2, 1)
# Mi riconduco dal predittore linare alle classi stimate, nel caso presente
# utilizzo come soglia di classificazione la proporzione campionaria delle 
# classi nell'insime di stima

kappa.vals.mcp <- matrix(NA, ncol(pred.train.class.mcp), 5)
# Inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for( i in 1:ncol(pred.train.class.mcp)){
  
  for(j in 1:5){
    ind.fold.out <- which(MCP_logistico_Kmeans$fold == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class.mcp[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.mcp[ind.fold.out,i], 
                   train$Etichette_Kmeans_K2[ind.fold.out])
      kappa.vals.mcp[i,j] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per il j-esimo fold
      
    }
  }
}

error.Lambda.mcp <- apply(kappa.vals.mcp,1,mean)
se.Lambda.mcp <- apply(kappa.vals.mcp,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k.mcp <- se.Lambda.mcp
`Kappa Cohen` <-  error.Lambda.mcp
`Log(Lambda)` <- log(MCP_logistico_Kmeans$lambda)
data.plot.cv.mcp <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans.MCP <- ggplot(data.plot.cv.mcp, 
                             mapping = aes(x = `Log(Lambda)`, 
                                           y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k.mcp)-0.005,
         max(`Kappa Cohen` + stderrcv.k.mcp)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` + stderrcv.k.mcp),
             shape = 95, size = 10) +
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.mcp),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.mcp,
                   xend = `Log(Lambda)`, 
                   yend = `Kappa Cohen` + stderrcv.k.mcp)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen CV MCP")
srs.acc_Kmeans.MCP
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 

lambda.k.mcp <- MCP_logistico_Kmeans$lambda[which.max(error.Lambda.mcp)]
# Definisco il lambda che massimizza Kappa

n.coef_MCP_Kmeans <- length(which(coef(MCP_logistico_Kmeans, s =  lambda.k.mcp)!=0))
# Numero di coefficienti tenuti dal modello

MCP_logistico_Kmeans.fit <- ncvreg(X_train,
                                    factor(train$Etichette_Kmeans_K2),
                                    family = "binomial",
                                    penalty = "MCP",
                                    trace = T,
                                    gamma = 20)


plot(MCP_logistico_Kmeans.fit)
abline(v=lambda.k.mcp)
# Grafico dei beta selezionati al variare di lambda

MCP_logistico_Kmeans.fit$lambda[MCP_logistico_Kmeans.fit$convex.min]
# Lambda minimo prima di raggiungere la regione di non convessit??
lambda.k.mcp
# Confronto con lambda scelta

y.hat.MCP_Kmeans <- predict(MCP_logistico_Kmeans, 
                            X_test, 
                            s = lambda.k.mcp, 
                            type = "response")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la 
# probabilitC  delle classi 

y.hat.MCP_Kmeans <- ifelse(y.hat.MCP_Kmeans > 0.6, 2, 1)
# Utilizzo come soglia la proporzione campionaria dei livelli nell'insieme di
# stima

Tabella.MCP_Kmeans <- tabella.sommario(y.hat.MCP_Kmeans, test$Etichette_Kmeans_K2)
Metriche_MCP_Kmeans <- indici.errore(Tabella.MCP_Kmeans)
# Salvo le varie metriche ottenute da tale modello 

# _____________ -----------------------------------------------------------
# Confronti Finali --------------------------------------------------------

Metriche_tot_Kmeans = sapply(grep("Metriche", ls(), value = T), get)
colnames(Metriche_tot_Kmeans) <- gsub("Metriche_","",colnames(Metriche_tot_Kmeans))

knitr::kable(Metriche_tot_Kmeans)
# Tabella finale con le metriche

Tab_tot_Kmeans = sapply(grep("Tabella", ls(), value = T), get)
colnames(Tab_tot_Kmeans) <- gsub("Tabella_","",colnames(Tab_tot_Kmeans))

kappa.finali <- c()
for(i in 1:4){
  kappa.finali <- c(kappa.finali,Kappa(matrix(Tab_tot_Kmeans[,i], 2,2, byrow=F))$Unweighted[1])
}
names(kappa.finali) = gsub("Tabella_","",colnames(Tab_tot_Kmeans))
kappa.finali <- data.frame(kappa.finali)
names(kappa.finali) <- "Kappa di Cohen"
knitr::kable(kappa.finali)
Tab_Kappa_Kmeans <- kappa.finali
# Calcolo del Kappa di Cohen per i modelli


Tab_ncoef.Kmeans <- sapply(grep("n.coef", ls(), value = T), get)
Tab_ncoef.Kmeans <- as.data.frame(Tab_ncoef.Kmeans)
colnames(Tab_ncoef.Kmeans) <- c("Numero di coefficienti diversi da 0")
knitr::kable(Tab_ncoef.Kmeans)
# Tabella con il numero di coefficienti tenuti dai modelli


tab.tot.kmeans <- rbind(Metriche_tot_Kmeans, Tab_ncoef.Kmeans$`Numero di coefficienti diversi da 0`, 
                        round(Tab_Kappa_Kmeans$`Kappa di Cohen`, 3))

row.names(tab.tot.kmeans)[10] <- "Coefficienti diversi da 0"
row.names(tab.tot.kmeans)[11] <- "Kappa di Cohen"


knitr::kable(tab.tot.kmeans)
# Tabella finale

save.image("LASSO_SCAD_MCP_Kmeans.RData")


# _____________ -----------------------------------------------------------

# SVM - Elastic  ----------------------------------------------------------

lambda.grid.svm <- exp(seq(-4.5,-2,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia C( il risultato di prove precedenti che
# non vengono riportate

validation <- which(train$Day %in% c(1,2,5))
# Poiche la funzione cv.sparseSVM non restituisce i predittori lineari per
# ogni fold, si usa come soluzione l'utilizo di un terzo insieme quello di 
# validazione per la scelta dei parametri, tale insieme comprenderC  le 
# osservazioni relative al primo secondo e quinto giorno

alpha.grid.svm <- c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
kappa.vals.alpha.svm <- rep(NA,length(alpha.grid.svm))
kappa.vals.lambda.svm <- rep(NA, length(lambda.grid.svm))
# Per far si che il modello abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializza
# un vettore che conterrC  la metrica Kappa sull'insieme di validazione

for(alpha in 1:length(alpha.grid.svm)){
  
  fit <- sparseSVM(X_train[-validation,],
                   ifelse(train$Etichette_Kmeans_K2==1,-1,1)[-validation],
                   lambda = lambda.grid.svm,
                   alpha = alpha.grid.svm[alpha])
  # Viene stimato un modello SVM con penalitC  Elastic-Net, sull'insieme di 
  # stima senza insieme di validazione
  
  pred.train.class.svm.cv <- predict(fit, X_train[validation,], type = "class")
  # Vengono calcolate le previsioni di tipo classe sull'insieme di validazione
  
  pred.train.class.svm.cv <- ifelse(pred.train.class.svm.cv == -1, 1, 2)
  # PoichC( in fase di stima i modelli avevano come variabile risposta una 
  # variabile codificata con i valori -1, 1, i valori previsiti vengono
  # riportati nella loro vera scala 1, 2.
  
  print(alpha)
  
  for( i in 1:ncol(pred.train.class.svm.cv)){
    
    if (length(unique(pred.train.class.svm.cv[,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel valore di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.svm.cv[,i], 
                   train$Etichette_Kmeans_K2[validation])
      kappa.vals.lambda.svm[i] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per l'i-esimo lambda
      
    }
  }
  
  kappa.vals.alpha.svm[alpha] <- max(kappa.vals.lambda.svm)
  # Salvo il valore massimo del Kappa per un dato alpha, al variare di lambda
}


`Kappa Cohen Validation` <-  unlist(kappa.vals.lambda.svm)
`Log(Lambda)` <- log(E.Net_SVM_Kmeans$lambda)
data.plot.kappa.val <- tibble(`Kappa Cohen Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans_SVM.kappa <- 
  ggplot(data.plot.kappa.val, 
         mapping = aes(x = `Log(Lambda)`, 
                       y = `Kappa Cohen Validation`)) +
  ylim(c(-0.29,0.2)) +
  geom_point(aes(color = "slateblue"), size = 1, shape=20) +
  ggtitle("Kappa Cohen Validation")+ theme(legend.position = "bottom", 
                                           axis.text = element_text(size = 8),
                                           axis.title = element_text(size = 8),
                                           legend.title=element_text(size=10), 
                                           legend.text=element_text(size=9))
srs.acc_Kmeans_SVM.kappa
# Visualizzo andamento kappa su insieme di validazione


kappa.vals.alpha.svm[which.max(kappa.vals.alpha.svm)]
# Massimo di Kappa al variare dell'alpha

best.alpha_Kmeans_SVM <- alpha.grid.svm[which.max(kappa.vals.alpha.svm)]
best.alpha_Kmeans_SVM
# Viene definito l'alpha per il quale si ottiene il Kappa maggiore

E.Net_SVM_Kmeans <- sparseSVM(X_train[-validation,],
                             ifelse(train$Etichette_Kmeans_K2==1,
                                    -1,1)[-validation],
                             lambda = lambda.grid.svm,
                             alpha = best.alpha_Kmeans_SVM)
# Stimo un modello SVM con penalitC  alpha data dalla massimizzazione del 
# Kappa tramite insieme di convalida

pred.train.class.svm <- predict(E.Net_SVM_Kmeans, 
                            X_train[validation,], 
                            type = "class")
# Calcolo valori previsti di tipo classe sull'insieme di validazione

pred.train.class.svm <- ifelse(pred.train.class.svm == -1, 1, 2)
# Riporto i livelli delle preveisioni a quelli di partenza

kappa.vals.lambda.svm <- rep(NA, length(lambda.grid.svm))
miss.vals.lambda.svm <- rep(NA, length(lambda.grid.svm))
# Inizializzo due vettori vuoti i quali conterranno i valori della metrica 
# kappa e dell'errore di errata classificazione ottenuti sull'insieme di 
# validazione

for( i in 1:ncol(pred.train.class.svm)){
  
  if (length(unique(pred.train.class.svm[,i])) != 1)
    # Condizione per la quale si valuta la metrica relativa a quel valore di
    # un lambda se e solo se le classi stimate non hanno solo un livello
  {
    
    tab <- table(pred.train.class.svm[,i],
                 train$Etichette_Kmeans_K2[validation])
    kappa.vals.lambda.svm[i] <- Kappa(tab)$Unweighted[1]
    
    miss.vals.lambda.svm[i] <- indici.errore(
      tabella.sommario(pred.train.class.svm[,i], 
                       train$Etichette_Kmeans_K2[validation])
    )$Errata.Classificazione
    # Si salvano le metriche all'interno delle liste vuote
    
  }
}

`Miss Validation` <-  unlist(miss.vals.lambda.svm)
`Log(Lambda)` <- log(E.Net_SVM_Kmeans$lambda)
data.plot.miss.val <- tibble(`Miss Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 


srs.acc_Kmeans_SVM.miss <- 
  ggplot(data.plot.miss.val, 
         mapping = aes(x = `Log(Lambda)`, 
                       y = `Miss Validation`)) +
  ylim(c(0,1)) +
  geom_point(aes(color = "slateblue"), size = 1, shape=20) +
  ggtitle("Misclassification Error Validation")+ theme(legend.position = "bottom", 
                                                        axis.text = element_text(size = 8),
                                                        axis.title = element_text(size = 8),
                                                        legend.title=element_text(size=10), 
                                                        legend.text=element_text(size=9))
# Visualizzo andamento errore di classificazione su insieme di validazione
srs.acc_Kmeans_SVM.miss


`Kappa Cohen Validation` <-  unlist(kappa.vals.lambda.svm)
`Log(Lambda)` <- log(E.Net_SVM_Kmeans$lambda)
data.plot.kappa.val <- tibble(`Kappa Cohen Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans_SVM.kappa <- 
  ggplot(data.plot.kappa.val, 
         mapping = aes(x = `Log(Lambda)`, 
                       y = `Kappa Cohen Validation`)) +
  ylim(c(-0.29,0.2)) +
  geom_point(aes(color = "slateblue"), size = 3) +
  ggtitle("Kappa Cohen Validation")
srs.acc_Kmeans_SVM.kappa
# Visualizzo andamento kappa su insieme di validazione

E.Net_SVM_Kmeans <- sparseSVM(X_train,
                             ifelse(train$Etichette_Kmeans_K2==1,
                                    -1,1),
                             lambda = lambda.grid.svm,
                             alpha = best.alpha_Kmeans_SVM)
# Ristimo il modello su tutto l'insieme di stima

pred.test.class.svm.train <- predict(E.Net_SVM_Kmeans, 
                            X_test, 
                            type = "class")
# Calcolo le previsioni di tipo classe

pred.test.class.svm.train <- ifelse(pred.test.class.svm.train == -1, 1, 2)
# Riporto i livelli delle preveisioni a quelli di partenza

kappa.vals.lambda.svm.tr <- rep(NA, length(lambda.grid.svm))
miss.vals.lambda.svm.tr <- rep(NA, length(lambda.grid.svm))
# Inizializzo due vettori vuoti i quali conterranno i valori della metrica 
# kappa e dell'errore di errata classificazione ottenuti sull'insieme di 
# validazione

for( i in 1:ncol(pred.test.class.svm.train)){
  
  if (length(unique(pred.test.class.svm.train[,i])) != 1)
    # Condizione per la quale si valuta la metrica relativa a quel valore di
    # un lambda se e solo se le classi stimate non hanno solo un livello
  {
    
    tab <- table(pred.test.class.svm.train[,i],
                 test$Etichette_Kmeans_K2)
    kappa.vals.lambda.svm.tr[i] <- Kappa(tab)$Unweighted[1]
    miss.vals.lambda.svm.tr[i] <-  indici.errore(
      tabella.sommario(pred.test.class.svm.train[,i], 
                       test$Etichette_Kmeans_K2))$Errata.Classificazione
    # Si salvano le metriche all'interno delle liste vuote
    
  }
}
`Miss Validation` <-  unlist(miss.vals.lambda.svm.tr)
`Log(Lambda)` <- log(E.Net_SVM_Kmeans$lambda)
data.plot.svm.miss.lam <- tibble(`Miss Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans_SVM.miss_tot <- srs.acc_Kmeans_SVM.miss + 
  geom_point(data = data.plot.svm.miss.lam,
             mapping = aes(x = `Log(Lambda)`, 
                           y =  `Miss Validation`,
                           color = "red"), size = 1, shape=20) +
  scale_color_identity(name = "Legenda",
                       breaks = c("slateblue", "red"),
                       labels = c("Validation","Test"),
                       guide = "legend")

srs.acc_Kmeans_SVM.miss_tot
# Visualizzo confronti tra andamento errore di classificazione su insieme
# di validazione e su insieme di verifica


`Kappa Cohen Validation` <-  kappa.vals.lambda.svm.tr
`Log(Lambda)` <- log(E.Net_SVM_Kmeans$lambda)
data.plot <- tibble(`Kappa Cohen Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kmeans_SVM.kappa_tot <- srs.acc_Kmeans_SVM.kappa + 
  geom_point(data = data.plot,
             mapping = aes(x = `Log(Lambda)`, 
                           y =  `Kappa Cohen Validation`,
                           color = "red"), size = 1, shape=20) +
  scale_color_identity(name = "Legenda",
                       breaks = c("slateblue", "red"),
                       labels = c("Validation","Test"),
                       guide = "legend")

srs.acc_Kmeans_SVM.kappa_tot
# Visualizzo confronti tra andamento kappa su insieme di validazione e su 
# insieme di verifica

base::save.image("SVM_Kmeans_K2_def1.RData")
# _____________ ----------------------------------------------------------

# Salvo risultati ---------------------------------------------------------

rm(train)
rm(test)
rm(X_train)
rm(X_test)
# Elimino dall'ambiente gli insiemi di verifica e stima utilizzati

base::save.image("Modelli_Kmeans_def1.RData")