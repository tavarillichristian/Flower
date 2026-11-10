########################################
# A. Pollice - STATISTICA MULTIVARIATA #
########################################
# TRACCIA 6 #
#############

# 1. I dati contenuti nel dataset iris di R riguardano 150 fiori riferiti a 3 specie di Iris. 
# Le variabili Sepal.Length, Sepal.Width, Petal.Length e Petal.Width rappresentano rispettivamente la lunghezza e la larghezza di sepali e petali ed il fattore Species individua la specie di appartenenza (setosa, versicolor e virginica). 
# Calcolate i valori caratteristici delle variabili del dataset.

# 2. Sintetizzate le osservazioni relative al fattore Species attraverso una tabella.

# 3. Costruite il nuovo dataset iris.1 estraendo casualmente (comando sample()) 75 osservazioni dalle 150. 
# Sintetizzate nuovamente, mediante una tabella ed un diagramma a torta le osservazioni relative al fattore Species, ma con riferimento al campione.

# 4. Costruite due coplot che mettano rispettivamente in relazione le dimensioni dei sepali e dei petali, ponendo la lunghezza sull?asse delle ascisse e condizionando rispetto ai livelli della variabile Species. 
# Cosa notate dall?osservazione di questi due grafici? (in alternativa potreste costruire due scatterplot con simboli e/o colori diversi a seconda della specie di appartenenza)

# 5. Effettuate l?analisi discriminante lineare dei dati campionati contenuti nel dataset iris.1 considerando il fattore di classificazione Species e le 4 variabili quantitative relative alle dimensioni dei fiori, creando un oggetto denominato iris.1.lda. 
# Analizzate l?output ottenuto con particolare riferimento ai coefficienti che mettono in relazione le variabili osservate con i fattori discriminanti ed ai poteri discriminanti. 
# Rappresentate graficamente le osservazioni classificate sul piano avente per coordinate le prime due funzioni discriminanti, con simboli diversi a seconda del gruppo di appartenenza. 
# Commentate i grafici ottenuti alla luce dei valori dei coefficienti e dei coplot di cui al punto 4.

#6. Utilizzate la funzione predict() per ottenere la classificazione delle 75 osservazioni non campionate del dataset iris tramite la funzione discriminante stimata iris.1.lda. 
# Costruite una tabella che metta in relazione le specie effettive delle osservazioni non campionate con quelle previste tramite la funzione predict(). 
# Rappresentate graficamente la tabella mediante un grafico a barre affiancate. 
# Commentate i risultati ottenuti e rappresentate graficamente le osservazioni classificate sul piano avente per coordinate le funzioni discriminanti, con simboli diversi a seconda del gruppo di appartenenza. 
# Commentate il grafico ottenuto.

# Extra

# 1bis. Effettuate ulteriori analisi discriminanti, attribuendo alle specie diverse specificazioni delle probabilit? a priori. 
# Verificare eventuali modifiche dell?output dell?analisi discriminante anche mediante rappresentazioni grafiche come al punto 6. 
# Commentate i risultati ottenuti.

# 2bis. Esaminate e commentate l?output ottenibile utilizzando la cross validation e non specificando le probabilit? a priori, creando un nuovo oggetto denominato iris.cv. 
# Costruite una tabella che sintetizzi la relazione tra le specie del campione previste in seguito all?utilizzo della cross validation e quelle effettive e rappresentate graficamente tale sintesi mediante un grafico a barre affiancate.


#1
data(iris)
str(iris)
summary(iris)
var(iris[,1:4])

#2
table(iris$species)

#3
?sample
train<-sample(1:150, 75)
iris.1<- iris[train, ]

tab1<- table(iris.1$Species)
tab1

pie(tab1, 
    col= c( "blue","green","red"), 
    main="Distribuzione delle Specie del Campione iris1 - 75 elementi")

#4
par(mfrow=c(1, 2))
coplot(Sepal.Width ~ Sepal.Length | Species, 
       data=iris.1,
       rows = 1)
coplot(Petal.Width ~ Petal.Length | Species, 
       data=iris.1,
       rows = 1)

#5
library(MASS)
iris.1.lda <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris.1)
iris.1.lda

iris.1.lda.pre <- predict(iris.1.lda)

par(mfrow=c(1, 1))
plot(iris.1.lda.pre$x[,1],iris.1.lda.pre$x[,2], 
     pch=16, 
     col=c("blue","green","red")[iris.1$Species],
     xlab="LDA1",
     ylab="LDA2",
     main="Rappresentazione sul Piano delle Prime due Funzioni Discriminanti") 
legend("bottom",
       legend=levels(iris.1$Species),
       pch=16,
       col=c("blue","green","red"))
#dal grafico emerge una forte dominanza della prima funzione discriminante: tutti i punti si distinguono sull'asse orizzontale
#la distinzione tra setosa dalle altre due specie è netta, per questo la regola discriminante distingue la specie setosa in modo quasi certo
#i gruppi di virginica e versicolor sono molto più vicini tra loro, sovrapponendosi per alcuni valori di LDA1, ma i due gruppi sono sufficientemente distinti

#6
iris.2<- iris[-train, ]
iris.pred<- predict(iris.1.lda, newdata= iris.2)

tab2<- table(iris.2$Species, iris.pred$class)
tab2

barplot(tab2, beside=T,
        col= c("blue","green","red"),
        main= "Confronto Specie Effettive vs Specie Previste",
        ylab= "frequenze",
        xlab= "classe Prevista")
legend("topleft",
       legend=levels(iris.2$Species),
       pch=16,
       col=c("blue","green","red"))

plot(iris.pred$x[,1],iris.pred$x[,2], 
     pch=16, 
     col=c("blue","green","red")[iris.2$Species],
     xlab="LDA1",
     ylab="LDA2",
     main="Funzioni Discriminanti delle Osservazioni non Campionate") 
legend("bottom",
       legend=levels(iris.2$Species),
       pch=16,
       col=c("blue","green","red"))
