rm(list = ls())

load("Modelli_Kaggle_def1.RData")
load("SCAD_Kaggle_def.RData")
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


# Lasso Logistico -------------------------------------------------------------

lambda.grid.lasso <- exp(seq(-7,-4,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia è il risultato di prove precedenti che
# non vengono riportate

registerDoMC(cores = 20)
# Al fine di parallelizzare la funzione cv.glmnet dichiaro il numero di core
# che intendo utilizzare per parallelizzare il modello 

Lasso_logistico_Kaggle <- cv.glmnet(X_train,
                                    factor(train$Etichette_Kaggle_K2),
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
# viene oltretutto fissato un seed per la riproducibilità dei risultati.
# Con l'etichetta presente però occorre fare una considerazione, ci troviamo 
# in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
# metrica come l'errore di errata classificazione potrebbe portare a conclusioni
# inestatte, per tanto viene impostato un keep = T all'interno del modello, 
# che permette di avere per ogni lambda la stima del predittore lineare delle
# unita statistiche che stanno nell'out of fold, cosi da riprodurre una 
# CV fatta a mano utilizzando i risultati ottenuti dalla funzione cv.glment

plot(Lasso_logistico_Kaggle)
# Visualizzo errore di previsione, si nota la scarsa efficacia della metrica

eta.lasso <- Lasso_logistico_Kaggle$fit.preval
pi.train.lasso <- exp(eta.lasso)/(1+exp(eta.lasso))
pred.train.class.lasso <- ifelse(pi.train.lasso > 1/2, 2, 1)
# Mi riconduco dal predittore linare alle classi stimate 

kappa.vals.lasso <- matrix(NA, ncol(pred.train.class.lasso), 5)
# Una metrica ragionevole per il caso presente di sbilanciamento è il Kappa di
# cholen, per tanto inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for( i in 1:ncol(pred.train.class.lasso)){
  
  for(j in 1:5){
    ind.fold.out <- which(Lasso_logistico_Kaggle$foldid == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class.lasso[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.lasso[ind.fold.out,i], 
                   train$Etichette_Kaggle_K2[ind.fold.out])
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
`Log(Lambda)` <- log(Lasso_logistico_Kaggle$lambda)
data.plot.cv.lasso <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kaggle.lasso <- ggplot(data.plot.cv.lasso, 
                               mapping = aes(x = `Log(Lambda)`, 
                                             y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k.lasso)-0.005,
         max(`Kappa Cohen` + stderrcv.k.lasso)+0.005))+
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` + stderrcv.k.lasso),
             shape = 95, size = 10) +
  geom_point(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.lasso),
             shape = 95, size = 10) +
  geom_segment(aes(x = `Log(Lambda)`, y = `Kappa Cohen` - stderrcv.k.lasso,
                   xend = `Log(Lambda)`, 
                   yend = `Kappa Cohen` + stderrcv.k.lasso)) +
  geom_point(col = "firebrick1", size = 3) +
  ggtitle("Kappa Cohen CV Lasso")
srs.acc_Kaggle.lasso
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 

lambda.k.lasso <- Lasso_logistico_Kaggle$lambda[which.max(error.Lambda.lasso)]
# Definisco il lambda che massimizza Kappa


n.coef_Lasso_Kaggle <- length(which(coef(Lasso_logistico_Kaggle, s = lambda.k.lasso)!=0))
# Numero di coefficienti diversi da 0

y.hat.lasso_Kaggle <- predict(Lasso_logistico_Kaggle, 
                              s =  lambda.k.lasso, 
                              newx = X_test, 
                              type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.lasso_Kaggle  <- tabella.sommario(y.hat.lasso_Kaggle,
                                          test$Etichette_Kaggle_K2)
Metriche_Lasso_Kaggle <- indici.errore(Tabella.lasso_Kaggle)
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
                   factor(train$Etichette_Kaggle_K2),
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
  # viene oltretutto fissato un seed per la riproducibilità dei risultati.
  # Con l'etichetta presente però occorre fare una considerazione, ci troviamo 
  # in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
  # metrica come l'errore di errata classificazione potrebbe portare a 
  # conclusioni inestatte, per tanto viene impostato un keep = T all'interno del
  # modello, che permette di avere per ogni lambda la stima del predittore 
  # lineare delle unita statistiche che stanno nell'out of fold, cosi da
  # riprodurre una CV fatta a mano utilizzando i risultati ottenuti dalla 
  # funzione cv.glment
  
  eta.enet <- fit$fit.preval
  pi.train.enet <- exp(eta.enet)/(1+exp(eta.enet))
  pred.train.class.enet <- ifelse(pi.train.enet > 1/2, 0, 1)
  # Mi riconduco dal predittore linare alle classi stimate 
  
  kappa.vals.enet.alpha <- matrix(NA, ncol(pred.train.class.enet), 5)
  # Una metrica ragionevole per il caso presente di sbilanciamento è il Kappa di
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
                     train$Etichette_Kaggle_K2[ind.fold.out])
        kappa.vals.enet.alpha[i,j] <- Kappa(tab)$Unweighted[1]
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

srs.acc_Kaggle.alpha.enet <- ggplot(data.plot.cv.enet.alpha, 
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
  ggtitle("Kappa Cohen CV Alpha - Elastic Net")
srs.acc_Kaggle.alpha.enet
# Visualizzazione di Kappa per i diversi alpha 

error.alpha.enet[which.max(error.alpha.enet)]
# Massimo di Kappa al variare dell'alpha

best.alpha_Kaggle.enet <- alpha.grid.enet[which.max(error.alpha.enet)]
best.alpha_Kaggle.enet
# Viene definito l'alpha per il quale si ottiene il Kappa maggiore

E.Net_logistico_Kaggle <- cv.glmnet(X_train,
                                    factor(train$Etichette_Kaggle_K2),
                                    type.measure = "class",
                                    family = "binomial",
                                    alpha = best.alpha_Kaggle.enet,
                                    nfolds = 5,
                                    foldid = fold,
                                    lambda = lambda.grid.enet,
                                    parallel = T,
                                    trace.it = 1,
                                    seed = 42,
                                    keep = T)
# Viene ristimato il modello logistico con penalita Elastic-Net fissata dal 
# best.alpha_Kaggle

plot(E.Net_logistico_Kaggle.enet)
# Visualizzo l'andamento dell'errore di classificazione per i diversi lambda
# del modello Elastic-Net

eta.enet <- E.Net_logistico_Kaggle$fit.preval
pi.train.enet <- exp(eta.enet)/(1+exp(eta.enet))
pred.train.class.enet.l <- ifelse(pi.train.enet > 1/2, 0, 1)
# Mi riconduco dal predittore linare alle classi stimate 

kappa.vals.enet.lambda <- matrix(NA, ncol(pred.train.class.enet), 5)
# Inizializzo una matrice vuota con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati

for( i in 1:ncol(pred.train.class.enet.l)){
  
  for(j in 1:5){
    ind.fold.out <- which(E.Net_logistico_Kaggle$foldid == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class.enet.l[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.enet.l[ind.fold.out,i], 
                   train$Etichette_Kaggle_K2[ind.fold.out])
      kappa.vals.enet.lambda[i,j] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per il j-esimo fold
      
    }
  }
}

error.Lambda.enet <- apply(kappa.vals.enet.lambda,1,mean)
se.Lambda.enet <- apply(kappa.vals.enet.lambda,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k.enet.lambda <- se.Lambda.enet
`Kappa Cohen` <-  error.Lambda.enet
`Log(Lambda)` <- log(E.Net_logistico_Kaggle$lambda)
data.plot.cv.enet.lambda <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kaggle.Enet.lambda <- ggplot(data.plot.cv.enet.lambda, 
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
  ggtitle("Kappa Cohen  CV")
srs.acc_Kaggle.Enet.lambda
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 

lambda.k.enet <- E.Net_logistico_Kaggle$lambda[which.max(error.Lambda.enet)]
# Definisco il lambda che massimizza Kappa


n.coef_E.Net_Kaggle <- length(which(coef(E.Net_logistico_Kaggle,s =  lambda.k.enet)!=0))
# Numero di coefficienti diversi da 0


y.hat.E.Net_Kaggle <- predict(E.Net_logistico_Kaggle,
                              s = lambda.k.enet,
                              newx = X_test,
                              type = "class")
# Calcolo le previsioni del modello ottenndo direttamente come tipo la classe

Tabella.E.Net_Kaggle <- tabella.sommario(test$Etichette_Kaggle_K2 ,
                                         y.hat.E.Net_Kaggle)
Metriche_E.Net_Kaggle <- indici.errore(Tabella.E.Net_Kaggle)
# Salvo le varie metriche ottenute da tale modello


# SVM - Elastic  ----------------------------------------------------------

lambda.grid.svm <- exp(seq(-4.5,-2,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia è il risultato di prove precedenti che
# non vengono riportate

validation <- which(train$Day %in% c(1,2,5))
# Poiche la funzione cv.sparseSVM non restituisce i predittori lineari per
# ogni fold, si usa come soluzione l'utilizo di un terzo insieme quello di 
# validazione per la scelta dei parametri, tale insieme comprenderà le 
# osservazioni relative al primo secondo e quinto giorno

alpha.grid.svm <- c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
kappa.vals.alpha.svm <- rep(NA,length(alpha.grid.svm))
kappa.vals.lambda.svm <- rep(NA, length(lambda.grid.svm))
# Per far si che il modello abbia un'adeguata penalita Elastic-Net si 
# prepara una possibile griglia di valori del parametro alpha, e si inzializza
# un vettore che conterrà la metrica Kappa sull'insieme di validazione


for(alpha in 1:length(alpha.grid.svm)){
  
  fit <- sparseSVM(X_train[-validation,],
                   ifelse(train$Etichette_Kaggle_K2==1,-1,1)[-validation],
                   lambda = lambda.grid.svm,
                   alpha = alpha.grid.svm[alpha])
  # Viene stimato un modello SVM con penalità Elastic-Net, sull'insieme di 
  # stima senza insieme di validazione
  
  pred.train.class.svm.al <- predict(fit, X_train[validation,], type = "class")
  # Vengono calcolate le previsioni di tipo classe sull'insieme di validazione
  
  pred.train.class.svm.al <- ifelse(pred.train.class.svm.al == -1, 1, 0)
  # Poichè in fase di stima i modelli avevano come variabile risposta una 
  # variabile codificata con i valori -1, 1, i valori previsiti vengono
  # riportati nella loro vera scala 1, 2.
  
  print(alpha)
  
  for( i in 1:ncol(pred.train.class.svm.al)){
    
    if (length(unique(pred.train.class.svm.al[,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel valore di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class.svm.al[,i], 
                   train$Etichette_Kaggle_K2[validation])
      kappa.vals.lambda.svm[i] <- Kappa(tab)$Unweighted[1]
      # Calcolo Kappa e salvo il risultato per l'i-esimo lambda
      
    }
  }
  
  kappa.vals.alpha.svm[alpha] <- max(kappa.vals.lambda.svm)
  # Salvo il valore massimo del Kappa per un dato alpha, al variare di lambda
}

`Kappa Cohen  CV` <-  kappa.vals.alpha.svm
`Alpha` <- alpha.grid.svm
data.plot.cv.svm.alpha <- tibble(`Kappa Cohen  CV`,`Alpha`)
# Vengono salvati i risultati ottenuti per i diversi alpha al fine di riprodurre
# un grafico per l'andamento dell'errore 

srs.acc_Kaggle_SVM <- ggplot(data.plot.cv.svm.alpha, 
                             mapping = aes(x = `Alpha`, 
                                           y = `Kappa Cohen  CV`)) +
  ylim(c(min(`Kappa Cohen  CV`)-0.01,
         max(`Kappa Cohen  CV`)+0.01))+
  geom_point(col = "slateblue", size = 3) +
  ggtitle("Kappa Cohen  CV  CV")
srs.acc_Kaggle_SVM
# Visualizzazione di Kappa per i diversi alpha 

kappa.vals.alpha.svm[which.max(kappa.vals.alpha.svm)]
# Massimo di Kappa al variare dell'alpha

best.alpha_Kaggle_SVM <- alpha.grid.svm[which.max(kappa.vals.alpha.svm)]
best.alpha_Kaggle_SVM
# Viene definito l'alpha per il quale si ottiene il Kappa maggiore

E.Net_SVM_Kaggle <- sparseSVM(X_train[-validation,],
                              ifelse(train$Etichette_Kaggle_K2==1,
                                     -1,1)[-validation],
                              lambda = lambda.grid.svm,
                              alpha = best.alpha_Kaggle_SVM)
# Stimo un modello SVM con penalità alpha data dalla massimizzazione del 
# Kappa tramite insieme di convalida

pred.train.class.SVM <- predict(E.Net_SVM_Kaggle, 
                                X_train[validation,], 
                                type = "class")
# Calcolo valori previsti di tipo classe sull'insieme di validazione

pred.train.class.SVM <- ifelse(pred.train.class.SVM == -1, 1, 0)
# Riporto i livelli delle preveisioni a quelli di partenza

kappa.vals.lambda.SVM <- rep(NA, length(lambda.grid.svm))
# Inizializzo un vettore vuoto lungo quanto la griglia di lambda

for( i in 1:ncol(pred.train.class.SVM)){
  
  if (length(unique(pred.train.class.SVM[,i])) != 1)
    # Condizione per la quale si valuta la metrica relativa a quel valore di
    # un lambda se e solo se le classi stimate non hanno solo un livello
  {
    
    tab <- table(pred.train.class.SVM[,i],
                 train$Etichette_Kaggle_K2[validation])
    kappa.vals.lambda.SVM[i] <- Kappa(tab)$Unweighted[1]
    # Calcolo Kappa e salvo il risultato per l'i-esimo lambda
    
    
    
  }
}


`Kappa Cohen Validation` <-  kappa.vals.lambda.SVM
`Log(Lambda)` <- log(lambda.grid.svm)
data.plot.lambda.SVM.validation <- tibble(`Kappa Cohen Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kaggle_SVM.kappa.validation <- 
  ggplot(data.plot.lambda.SVM.validation, 
         mapping = aes(x = `Log(Lambda)`, 
                       y = `Kappa Cohen Validation`)) +
  ylim(c(-0.05,0.05)) +
  geom_point(col = "slateblue", size = 3) +
  ggtitle("Kappa Cohen Validation")
srs.acc_Kaggle_SVM.kappa.validation
# Visualizzo andamento kappa su insieme di validazione

lambda.k.SVM <- lambda.grid.svm[which.max(kappa.vals.lambda.SVM)]
# Salvo il miglior lambda

E.Net_SVM_Kaggle <- sparseSVM(X_train,
                              ifelse(train$Etichette_Kaggle_K2==1,
                                     -1,1),
                              lambda = lambda.k.SVM,
                              alpha = best.alpha_Kaggle_SVM)
# Ristimo il modello su tutto l'insieme di stima

n.coef_SVM_Kaggle <- length(which(coef(E.Net_SVM_Kaggle, lambda=lambda.k.SVM)!=0))
# Numero di coefficienti diversi da 0


y.hat.E.Net.SVM_Kaggle <- predict(E.Net_SVM_Kaggle, 
                                  X_test, 
                                  lambda = lambda.k.SVM,
                                  type = "class")
# Calcolo le previsioni di tipo classe

y.hat.E.Net.SVM_Kaggle <- ifelse(y.hat.E.Net.SVM_Kaggle == -1, 1, 0)
# Riporto i livelli delle preveisioni a quelli di partenza
table(y.hat.E.Net.SVM_Kaggle)

Tabella.E.Net.SVM_Kaggle <- tabella.sommario(y.hat.E.Net.SVM_Kaggle, test$Etichette_Kaggle_K2 )
Metriche_E.Net.SVM_Kaggle <- indici.errore(Tabella.E.Net.SVM_Kaggle)
# Salvo le varie metriche ottenute da tale modello

# _____________ -----------------------------------------------------------
# Confronti Finali --------------------------------------------------------

Metriche_tot_Kaggle = sapply(grep("Metriche", ls(), value = T), get)
colnames(Metriche_tot_Kaggle) <- gsub("Metriche_","",colnames(Metriche_tot))
knitr::kable(Metriche_tot_Kaggle)
# Creazione della tabella con le metriche

Tab_tot_Kaggle = sapply(grep("Tabella", ls(), value = T), get)
colnames(Tab_tot_Kaggle) <- gsub("Tabella_","",colnames(Tab_tot_Kaggle))

kappa.finali <- c()
for(i in 1:4){
  kappa.finali <- c(kappa.finali,Kappa(matrix(Tab_tot[,i], 2,2, byrow=F))$Unweighted[1])
}

names(kappa.finali) = gsub("Tabella_","",colnames(Tab_tot))
kappa.finali <- data.frame(kappa.finali)
names(kappa.finali) <- "Kappa di Cohen"
knitr::kable(kappa.finali)
Tab_Kappa_Kaggle <- kappa.finali
# Calcolo del Kappa di Cohen per i modelli

Tab_ncoef.Kaggle <- sapply(grep("n.coef", ls(), value = T), get)
# Tabella con il numero di coefficienti tenuti dai modelli
Tab_ncoef.Kaggle <- as.data.frame(Tab_ncoef.Kaggle)
colnames(Tab_ncoef.Kaggle) <- c("Numero di coefficienti diversi da 0")
knitr::kable(Tab_ncoef.Kaggle)
# Tabella con il numero di coefficienti diversi da 0

tab.tot.kaggle <- rbind(Metriche_tot_Kaggle, 
                        Tab_ncoef.Kaggle$`Numero di coefficienti diversi da 0`,
                        round(Tab_Kappa_Kaggle[1:3],3))

tab.tot.kaggle
row.names(tab.tot.kaggle)[10] <- "Coefficienti diversi da 0"
row.names(tab.tot.kaggle)[11] <- "Kappa di Cohen"
knitr::kable(tab.tot.kaggle)
# Tabella Finale

# _____________ -----------------------------------------------------------
# SCAD --------------------------------------------------------------------

lambda.grid <- exp(seq(-3,-7,l=100))
# Definisco una griglia di lambda che mi permetta di esplorare un sottospazio
# ben definito di valori, tale griglia è il risultato di prove precedenti che
# non vengono riportate

SCAD_logistico_Kaggle <- cv.ncvreg(X_train,
                                   factor(train$Etichette_Kaggle_K2),
                                   family = "binomial",
                                   penalty = "SCAD",
                                   nfolds = 5,
                                   fold = fold,
                                   trace = T,
                                   lambda = lambda.grid,
                                   gamma = 20,
                                   seed = 42,
                                   returnY = T)
# Viene stimato un modello logistico con penalita SCAD, in CV a 5 fold con 
# metrica di ottimizzazione la verosimiglianza negativa binomiale,
# viene oltretutto fissato un seed per la riproducibilità dei risultati.
# Con l'etichetta presente però occorre fare una considerazione, ci troviamo 
# in un caso di sbilanciamento per tanto stimare i modelli utilizzando una 
# metrica come l'errore di errata classificazione potrebbe portare a conclusioni
# inestatte, per tanto viene impostato un returnY = T all'interno del modello, 
# che permette di avere per ogni lambda la stima del predittore lineare delle
# unita statistiche che stanno nell'out of fold, cosi da riprodurre una 
# CV fatta a mano utilizzando i risultati ottenuti dalla funzione cv.ncvreg

plot(SCAD_logistico_Kaggle)
# Visualizzo andamento della verosimiglianza negativa per i vari lambda

eta <- SCAD_logistico_Kaggle$Y
pi.train <- exp(eta)/(1+exp(eta))
pred.train.class <- ifelse(pi.train > 0.70, 0, 1)
# Mi riconduco dal predittore linare alle classi stimate, nel caso presente
# utilizzo come soglia di classificazione la proporzione campionaria delle 
# classi nell'insime di stima

kappa.vals.scad.kaggle <- matrix(NA, ncol(pred.train.class), 5)
miss.vals.scad.kaggle <- matrix(NA, ncol(pred.train.class), 5)
# Inizializzo due matrici vuote con numero di righe pari al 
# numero di lambda, e numero di colonne pari ai fold utilizzati


for( i in 1:ncol(pred.train.class)){
  
  for(j in 1:5){
    ind.fold.out <- which(SCAD_logistico_Kaggle$fold == j)
    # Definisco il fold corrente
    
    if (length(unique(pred.train.class[ind.fold.out,i])) != 1)
      # Condizione per la quale si valuta la metrica relativa a quel fold, di
      # un lambda se e solo se le classi stimate non hanno solo un livello
    {
      
      tab <- table(pred.train.class[ind.fold.out,i], 
                   train$Etichette_Kaggle_K2[ind.fold.out])
      kappa.vals.scad.kaggle[i,j] <- Kappa(tab)$Unweighted[1]
      miss.vals.scad.kaggle[i,j] <- indici.errore(
        tabella.sommario(
          pred.train.class[ind.fold.out,i],
          train$Etichette_Kaggle_K2[ind.fold.out]
        )
      )$Errata.Classificazione
      # Calcolo metriche e salvo il risultato per il j-esimo fold
      
      
    }
  }
}

error.Lambda.k1 <- apply(kappa.vals.scad.kaggle,1,mean)
se.Lambda.k1 <- apply(kappa.vals.scad.kaggle,1,sd)
# Calcolo Kappa medio per ogni fold e relativa deviazione standard

stderrcv.k.lam1 <- se.Lambda.k1
`Kappa Cohen` <-  error.Lambda.k1
`Log(Lambda)` <- log(SCAD_logistico_Kaggle$lambda)
data.plot.cv.k.lam1 <- tibble(`Kappa Cohen`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kaggle.SCAD.k <- ggplot(data.plot.cv.k.lam1, 
                                mapping = aes(x = `Log(Lambda)`, y = `Kappa Cohen`)) +
  ylim(c(min(`Kappa Cohen` - stderrcv.k)-0.005,
         max(`Kappa Cohen` + stderrcv.k)+0.005))+
  geom_line(aes(color = "firebrick1"), size = 1, shape=1) +
  geom_ribbon(aes(ymin = `Kappa Cohen` - stderrcv.k, ymax = `Kappa Cohen` + stderrcv.k)
              , alpha = 0.1) +
  ggtitle("Kappa Cohen CV - SCAD")+ 
  theme(legend.position = "bottom", axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=9))
# Visualizzazione della metrica Kappa per i diversi lambda, in scala logaritmica 


error.Lambda.miss <- apply(miss.vals.scad.kaggle,1,mean)
se.Lambda.miss <- apply(miss.vals.scad.kaggle,1,sd)
# Calcolo Miss medio per ogni fold e relativa deviazione standard

stderrcv.k.l.miss <- se.Lambda.miss
`Miss` <-  error.Lambda.miss
`Log(Lambda)` <- log(SCAD_logistico_Kaggle$lambda)
data.plot.cv.miss <- tibble(`Miss`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di 
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kaggle.SCAD.miss <- ggplot(data.plot.cv.miss, 
                                   mapping = aes(x = `Log(Lambda)`, 
                                                 y = `Miss`)) +
  ylim(c(min(`Miss` - stderrcv.k.l.miss)-0.005,
         max(`Miss` + stderrcv.k.l.miss)+0.005))+
  geom_line(aes(color = "firebrick1"), size = 1, shape=20) +
  geom_ribbon(aes(ymin = `Miss` - stderrcv.k.l.miss, ymax = `Miss` + stderrcv.k.l.miss)
              , alpha = 0.1) + 
  ggtitle("Err. di classificazione CV - SCAD")+ 
  theme(legend.position = "bottom", axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.title=element_text(size=10), 
        legend.text=element_text(size=9)) + 
  labs(y = "Errore di classificazione", x = "Log(lambda)")
# Visualizzazione della metrica Miss per i diversi lambda, in scala logaritmica 

eta <- predict(SCAD_logistico_Kaggle, 
               X_test, 
               type = "link",
               lambda = lambda.grid)
pi.test <- exp(eta)/(1+exp(eta))
pred.test.class <- ifelse(pi.test > 0.7, 0, 1)
# Mi riconduco dal predittore linare alle classi stimate sull'insieme di 
# verifica, nel caso presente utilizzo come soglia di classificazione 
# la proporzione campionaria delle classi nell'insime di stima

kappa.vals.scad.kaggle.lambda <- rep(NA, length(lambda.grid))
miss.vals.scad.kaggle.lambda <- rep(NA, length(lambda.grid))
# Inizializzo due vettori vuoti i quali conterranno i valori della metrica 
# kappa e dell'errore di errata classificazione ottenuti sull'insieme di 
# verifica

for( i in 1:ncol(pred.test.class)){
  
  if (length(unique(pred.test.class[,i])) != 1)
    # Condizione per la quale si valuta la metrica relativa a quel valore di
    # un lambda se e solo se le classi stimate non hanno solo un livello
  {
    
    tab <- table(pred.test.class[,i],
                 test$Etichette_Kaggle_K2)
    kappa.vals.scad.kaggle.lambda[i] <- Kappa(tab)$Unweighted[1]
    miss.vals.scad.kaggle.lambda[i] <-  indici.errore(
      tabella.sommario(pred.test.class[,i], 
                       test$Etichette_Kaggle_K2))$Errata.Classificazione
    # Si salvano le metriche all'interno delle liste vuote
    
  }
}


`Kappa Cohen Validation` <-  kappa.vals.scad.kaggle.lambda
`Log(Lambda)` <- log(SCAD_logistico_Kaggle$lambda)
data.plot.kl <- tibble(`Kappa Cohen Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 


srs.acc_Kaggle.SCAD.k2 <- srs.acc_Kaggle.SCAD.k + 
  geom_line(data = data.plot,
            aes(x = `Log(Lambda)`, 
                y =  `Kappa Cohen Validation`,
                color = "slateblue"), size = 1, shape=20) +
  scale_color_identity(name = "",
                       breaks = c("slateblue", "firebrick1"),
                       labels = c("Test","CV"),
                       guide = "legend")
# Visualizzo confronti tra andamento kappa su cv e su 
# insieme di verifica

`Miss Validation` <-  unlist(miss.vals.scad.kaggle.lambda)
`Log(Lambda)` <- log(SCAD_logistico_Kaggle$lambda)
data.plot <- tibble(`Miss Validation`,`Log(Lambda)`)
# Vengono salvati i risultati ottenuti per i diversi lambda al fine di
# riprodurre un grafico per l'andamento dell'errore 

srs.acc_Kaggle.SCAD.miss2 <- srs.acc_Kaggle.SCAD.miss + 
  geom_line(data = data.plot,
            mapping = aes(x = `Log(Lambda)`, 
                          y =  `Miss Validation`,
                          color = "slateblue"),size = 1, shape=20) +
  scale_color_identity(name = "",
                       breaks = c("slateblue", "firebrick1"),
                       labels = c("Test","CV"),
                       guide = "legend")
# Visualizzo confronti tra andamento errore di classificazione su cv
# e su insieme di verifica


# _____________ -----------------------------------------------------------
# Salvo risultati ---------------------------------------------------------

base::save.image("SCAD_Kaggle_def.RData")


# _____________ -----------------------------------------------------------
# Salvo risultati ---------------------------------------------------------


rm(train)
rm(test)
rm(X_train)
rm(X_test)
# Elimino dall'ambiente gli insiemi di verifica e stima utilizzati

base::save.image("Modelli_Kaggle_def1.RData")

