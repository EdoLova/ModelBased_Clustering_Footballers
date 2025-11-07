#Analisi Tesi Triennale Edo

# Provo con tutte e 20 le squadre della Serie A 2022-2023
#setwd("~/Desktop/0TesiTriennaleStatistica")
#dati <- read_excel("GiocatoriFbrefWhoScored.xlsx")
#dati <- read_excel("Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
#dati <- read_excel("~/Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
#dati <- read_excel("Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
#dati<- read_excel("Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
#dati <- read_excel("Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
#dati <- read_excel("Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
#dati <- read_excel("Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
dati <- read_excel("Desktop/0TesiTriennaleStatistica/GiocatoriFbrefWhoScored.xlsx")
#dati <- read_excel("GiocatoriFbrefWhoScored.xlsx")
head(dati)
str(dati)
dati=as.data.frame(dati)
str(dati)
rownames(dati)=dati[,1]
View(dati) 
dim(dati)
head(dati)
str(dati) #chr sia MinutiGiocati/90 min sia P100Spazzate

###############################################################
#Normalizzo con P90 e P100
head(dati[,3]) #minutiGiocati / 90 min

dati$`MinutiGiocati/90 min` =as.numeric(unlist(dati$`MinutiGiocati/90 min`))
str(dati) ##ok adesso è numeric

dati$P100Spazzate=as.numeric(unlist(dati$P100Spazzate))
str(dati)#ok adesso è numeric

minGiocDiv90=dati[,3] #creo 1 v cosi poi faccio diviso quella v per standardizzare/normalizzare

#controllo che giocatori presewnti nel datset abbiano "minutiGiocati/90min" ALMENO PARI A 5 cioe 450:90
#altrimenti se ce ne sono devo eliminare quei giocatori dal dataset
head(dati)
dati$`MinutiGiocati/90 min` <5 #ok non ce ne sono
any(dati$`MinutiGiocati/90 min` <5) ## ok non ce ne sono #any restituisce True solo se ce almeno 1 true 
which(dati$`MinutiGiocati/90 min` <5) #ok ottimo

# tocchi/100
head(dati[,5:6])
tocchiDiv100=dati[,5]
tocchiDiv100

dim(dati)
length(tocchiDiv100) #ok stessa lunghezza

head(dati)
#dataset senza colonne Squadra(2),minGioc/90(3), tocchi(4), tocchi/100(5)
datiSoloVariab=dati[,-c(2,3,4,5)]
head(datiSoloVariab) #tengo nella prim colonna nome giocatore

dim(datiSoloVariab) #406  25

dati[,1] #variabile nomi giocatori che inserisco dopo
datiSoloVariabP90P100Da2a5 =datiSoloVariab[,2:5]/c(rep(tocchiDiv100,4))
datiSoloVariabP90P100la6 =datiSoloVariab[,6]/c(rep(minGiocDiv90,1))
datiSoloVariabP90P100Da7a8 =datiSoloVariab[,7:8]/c(rep(tocchiDiv100,2))
datiSoloVariabP90P100Da9a10 =datiSoloVariab[,9:10]/c(rep(minGiocDiv90,2))
datiSoloVariabP90P100la11 =datiSoloVariab[,11]/c(rep(tocchiDiv100,1))
datiSoloVariabP90P100Da12a18 =datiSoloVariab[,12:18]/c(rep(minGiocDiv90,7))
datiSoloVariabP90P100Da19a20 =datiSoloVariab[,19:20]/c(rep(tocchiDiv100,2))
datiSoloVariabP90P100Da21a25 =datiSoloVariab[,21:25]/c(rep(minGiocDiv90,5))

#dataset normalizzato P90 e P100
datiSoloVariabP90P100=cbind(datiSoloVariabP90P100Da2a5,datiSoloVariabP90P100la6,datiSoloVariabP90P100Da7a8,
                            datiSoloVariabP90P100Da9a10,datiSoloVariabP90P100la11,datiSoloVariabP90P100Da12a18,
                            datiSoloVariabP90P100Da19a20,datiSoloVariabP90P100Da21a25)                       
dim(datiSoloVariabP90P100) #406  24 #ok perche manca la 1a v. Nome giocatore
head(datiSoloVariabP90P100)

str(datiSoloVariabP90P100) #tutte num 

#potevo usare mutate
#o creo nuove varibili

###########################################################################
# prossimo passo:
#standrdizzare tutte le statistiche su stessa scala con metodo STNDARD SCALING:
#metodo STNDARD SCALING:valore medio=0, dev.stndrd=1
datiStandardScaling=scale(datiSoloVariabP90P100)
head(datiStandardScaling)
mean(datiStandardScaling) #ok vicino a 0
sd(datiStandardScaling) #0.9956328 #ok vicino a 1
range(datiStandardScaling) #-2.096387  5.928104 #à#nuova -2.228585  7.073488
hist(datiStandardScaling,prob=T,nclass=100)
curve(dnorm(x),col="red",add=T)

##########################################################
#prossimo passo: RIDUZ DIMENSIONALITA 
#con algoritmo UMAP si arriva a 2 dim e controllo manuale
#install.packages("umap")
library(umap)
?umap.defaults

umap

##
  

for (i in 1:10) {
  datiRiduzDimUMAP <- umap(datiStandardScaling, n_components = 2, n_neighbors = 30, min_dist = 0.001, random_state = i)
  a.mclust <- Mclust(datiRiduzDimUMAP$layout)
  a.mclustICL <- mclustICL(datiRiduzDimUMAP$layout)
  
  # Correggo la condizione if
  if (unique(which.max(apply(a.mclustICL, 1, max))) == a.mclust$G) {
    print(a.mclust$G)
    print(i)
  }
}

# numero cluster       #9 7 7 7 6
# numero random state  #1 4 5 6 10



  
##


#set.seed(123) #uso il random state dentro la funzione umap
datiRiduzDimUMAP= umap(datiStandardScaling,n_components=2, n_neighbors=30, min_dist=0.001, random_state=1)  #min_dist=0.000000001 #n_neighbours chiedo che numero # n_neighbors in UMAP è 15 #minimo 5 privilegia individuazion gruppi piccoli, massmo 50 circa privilegia gruppi grandi 
# scelgo #n_neighbors=30 #per preservare le strutture globali utili per il clustering, riducendo il rischio di sovra-interpretare dettagli locali come cluster separati
#random state # scelgo 22 #bic 7 icl 7 #23 bic 7, icl 6
datiRiduzDimUMAP #conterra' le coordinate ridotte dei tuoi dati in 1 spazio a 2 dimensioni
#n_neighbors chiedo che numero?

datiRiduzDimUMAP$layout #Florian Thauvin  1.286868431 -1.626768488 #random_state(1)
datiRiduzDimUMAP$data
datiRiduzDimUMAP$knn$indexes
datiRiduzDimUMAP$knn$distances
datiRiduzDimUMAP$config

#1. #diagramma dispersione #con colore in base a squadra, se volgio specifico io i colori
plot(datiRiduzDimUMAP$layout[,1],datiRiduzDimUMAP$layout[,2],pch=16,col=as.factor(dati$Squadra) ,xlab = "UMAP 1",ylab="UMAP 2")
#vedo che si individuano 4 macro cluster ??

dim(datiRiduzDimUMAP$layout)
class(datiRiduzDimUMAP)

#2. # heatmap delle coordinate
heatmap(datiRiduzDimUMAP$layout)

#3. #  diagr a dispersione delle coordinate ridotte
library(ggplot2)
ggplot(data=as.data.frame(datiRiduzDimUMAP$layout), aes(x=V1,y=V2))+
  geom_point()+
  labs(x="UMAP 1",y="UMAP 2")+
  theme_minimal()

#4. #metto etichetta nomiGiocatori ai punti nel diagramma dispersione
class(datiRiduzDimUMAP$layout)
dati_com_df=as.data.frame(datiRiduzDimUMAP$layout) 
class(dati_com_df)

#CREO GRAFICO: data=dati_completi_df
nomi_righe=row.names(dati_com_df)
nomi_squadre=dati[,2]


grafico=ggplot(data=dati_com_df, aes(x=V1,y=V2,label=nomi_righe,col=nomi_squadre) )+  #forse colore di nomi squadre non serve #col=nomi_squadre #dentro aes
  geom_point()+
  geom_text(size=3,vjust=-0.5)+ #aggiungi le etichette dei nopmi dei giocatori
  labs(title = "Diagramma di Dispersione" ,x="UMAP 1",y="UMAP 2")+ #eticheete degli assi
  theme_minimal()+
  scale_color_manual(values = colori_squadre)+ # applica i colori personalizzati
  theme(
    plot.title = element_text(size = 25, hjust = 0.5, vjust = 1, face = "bold")
  )
grafico
#giocatori con ruoli simili sono vicini nel grafico


ggplot(matrice_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title="Heatmap",x = "Cluster", y = "Variabili", fill = "Scala Valori") +
  theme_minimal() +
  scale_y_discrete(labels = nuovi_nomi_variabili) +
  scale_x_discrete(labels =c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
                             "5-Finalizzatore","6-Equilibratore","7-Ruba Palloni","8-Marcatore","9-Laterale Creatore")) + #c("cluster1","cluster2","cluster3","cluster4","cluster5","cluster6",
  #"cluster7","cluster8","cluster9")
  theme(
    axis.text.x = element_text(angle = 20, hjust = 0.9, size = 17),  # Ingrandisci le etichette dell'asse x (cluster)
    axis.text.y = element_text(size = 15),  # Ingrandisci le etichette dell'asse y (variabili)
    axis.title.x = element_text(size = 16),  # Ingrandisci il titolo dell'asse x
    axis.title.y = element_text(size = 16),  # Ingrandisci il titolo dell'asse y
    plot.title = element_text(size = 25, hjust = 0.5, vjust = 1, face = "bold"),# Centra e ingrandisci il titolo
    panel.grid.major.x = element_line(color = "grey", size = 0.5),  # Aggiungi griglie verticali
    #panel.grid.major.y = element_line(color = "grey", size = 0.5) ,  # Aggiungi griglie orizzontali
    legend.title = element_text(size = 14)
  )


colori_squadre <- c(
  "Milan" = "#FF0000",            # rosso
  "Atalanta" = "#1C1C3A",  # blu abisso
  "Bologna" = "#8B0000",         # rosso scuro
  "Cremonese" = "#808080",       # grigio
  "Empoli" = "#00BFFF",          # azzurro
  "Fiorentina" = "#800080",      # viola
  "Hellas Verona" = "#FFD700",    # giallo scuro
  "Inter" = "#00008B" ,           # blu scuro         
  "Juventus" = "#A9A9A9" ,        # grigio scuro
  "Lazio" = "#87CEEB",           # celeste
  "Lecce" = "#FFC300",            # giallo quasi arancione
  "Monza" = "#FF4500",           # rosso vivo
  "Napoli" = "#00CED1",          # azzurro cielo
  "Roma" = "#FF7F50",            # arancione
  "Salernitana" = "#A52A2A",     # marrone
  "Sampdoria" = "#4169E1",       # bluette
  "Sassuolo" = "#008000",        # verde
  "Spezia" = "#000000",          # nero
  "Torino" = "#A50000",          # granata
  "Udinese" = "#1C1C1C"  # grigio quasi nero
)

##########


#########
#prossimo passo: MOD (no Bayesian) Gaussian Mixture
library(mclust)
?Mclust

a.mclust=Mclust(datiRiduzDimUMAP$layout)
a.mclust$BIC  #EEI,9 #random_state(1)   # vve 7 random_state(22)
a.mclust$modelName #"EEI"
a.mclust$bic
a.mclust$icl

a.mclust$loglik
a.mclust$df

a.mclust$G #9
a.mclust$parameters
a.mclust$classification
a.mclust$uncertainty

plot(a.mclust, what="BIC")
plot(a.mclust, what="BIC", ylim=c(-2650,-2500)) #EEI in 9 risulta piu alto

# icl prediligie se piu importante defnire i cluster piuttosto checomponenti #sulla tesi ancora non l ho scritto AGGIUNGO IN CASO
a.mclustICL=mclustICL(datiRiduzDimUMAP$layout)
plot(a.mclustICL)
plot(a.mclustICL,ylim=c(-2900,-2500))

a.mclustICL #EEI,9 cluster #icl #random_state(1)
class(a.mclustICL)

#
?predict
predict(a.mclust)
?predict.Mclust()
predict.Mclust(a.mclust)

table(predict.Mclust(a.mclust)$classification) #freq assoluta ciascun cluster
#1  2  3  4  5  6  7  8  9 
#44 43 48 30 59 37 46 54 45 
hist(table(predict.Mclust(a.mclust)$classification))


# Carica le librerie necessarie
library(ggplot2)
# Supponiamo che 'a.mclust' sia il tuo modello Mclust già adattato
classific <- predict(a.mclust)$classification
# Crea una tabella dei conteggi per ogni cluster
conteggi_cluster <- as.data.frame(table(classific))
# Rinomina le colonne per maggiore chiarezza
colnames(conteggi_cluster) <- c("Cluster", "Conteggio")

# Definisci i nomi dei cluster e i colori
noms_cluster=c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
  "5-Finalizzatore","6-Equilibratore","7-Ruba Palloni","8-Marcatore","9-Laterale Creatore")
#noms_cluster <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9")
colours <- c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink")  # Aggiungi più colori se hai più cluster

# Aggiungi i nomi dei cluster
conteggi_cluster$Cluster <- factor(conteggi_cluster$Cluster, levels = 1:length(noms_cluster), labels = noms_cluster)

# Crea l'istogramma usando ggplot
ggplot(conteggi_cluster, aes(x = Cluster, y = Conteggio, fill = Cluster)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Conteggio), position = position_stack(vjust = 0.5), color = "white",size=8) +  # Aggiungi le etichette delle frequenze
  scale_fill_manual(values = colours) +  # Applica i colori personalizzati
  labs(title = "Istogramma delle frequenze assolute dei giocatori per cluster",
       x = "Cluster",
       y = "Conteggio",
       size=20) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),  # Centra il titolo e ingrandisci la dimensione
    axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1, size = 18),
    axis.title.x = element_text(size = 17),  # Ingrandisci il nome dell'asse X ("Cluster")
    axis.title.y = element_text(size = 17),
    #legend.position = "bottom",  # Posiziona la legenda in basso
    #legend.direction = "horizontal", # Dispone la legenda in orizzontale
    #legend.text = element_text(size = 12),  # Aumenta la dimensione del testo della legenda
    #legend.title = element_text(size = 14)  # Aumenta la dimensione del titolo della legenda

    legend.position = "none")  # Rimuove la legenda  # Ruota e ingrandisci le etichette dei cluster
  





###


# Effettua di nuovo il plot
plot(a.mclust, what = "classification", col=c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"))

# Titolo aggiuntivo con 'title()'
title(main = "Classification Plot",  # Titolo del grafico
      col.main = "black",  # Colore del titolo
      #font.main = 2,  # Grassetto
      cex.main = 1.5)  # Ingrandisci il titolo

# Aggiungi la legenda
#legend("bottomleft", legend = paste("Cluster", 1:length(unique(a.mclust$classification))), 
       #col = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"), pch=10)

# Definisci i simboli per ciascun cluster
symbols <- c(16,0,17,3,15,4,1,8,2)  # Simboli per ogni cluster

# Aggiungi la legenda in basso a sinistra
legend("bottomleft", 
       legend = paste(c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
                        "5-Finalizzatore","6-Equilibratore","7-Ruba Palloni","8-Marcatore","9-Laterale Creatore")
                    ),  # Etichette della legenda
       col = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"),  # Colori dei cluster
       pch = symbols,  # Applica simboli per ogni cluster
       #bty = "n",  # Rimuove il riquadro attorno alla legenda
       pt.cex = 1.5,  # Dimensione dei simboli della legenda
       cex = 0.95)  # Dimensione del testo della legenda


# Load necessary libraries
library(ggplot2)

# Plot with ggplot2 #non serve
ggplot(as.data.frame(datiRiduzDimUMAP$layout), aes(x = V1, y = V2, color = as.factor(predict.Mclust(a.mclust)$classification))) +
  geom_point(size = 3) +
  labs(x = "UMAP 1", y = "UMAP 2", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right") # Optional, adjusts the legend position
###



str(a.mclust)

predict.Mclust(a.mclust)$z

((predict.Mclust(a.mclust)$z >0.33) & (predict.Mclust(a.mclust)$z <0.67))
which(((predict.Mclust(a.mclust)$z >0.33) & (predict.Mclust(a.mclust)$z <0.67)))

class(predict.Mclust(a.mclust)$z)

#trovo i giocatori ibridi:
which(apply(((predict.Mclust(a.mclust)$z >0.33) & (predict.Mclust(a.mclust)$z <0.67)), 1, function(x) any(x == TRUE)))
lista_indici_ibr=unique(which(apply(((predict.Mclust(a.mclust)$z >0.33) & (predict.Mclust(a.mclust)$z <0.67)), 1, function(x) any(x == TRUE))))
lista_indici_ibr
##metto indici corretti dei giocatori ibridi:
round(predict.Mclust(a.mclust)$z[lista_indici_ibr,],2)

cbind(ds_vmedie,round(datiSoloVariabP90P100,2))[c("Sergej Milinković-Savić"),])
cbind(t(ds_vmedie)[,c(1,6)],round(t(datiSoloVariabP90P100[c("Sergej Milinković-Savić"),]),2))
cbind(t(ds_vmedie)[,c(1,6)],round(t(datiSoloVariabP90P100[c("Patrick Ciurria"),]),2))

t(ds_vmedie)[,c(1,6)]



str(a.mclust$classification)
clus1=names(a.mclust$classification[a.mclust$classification==1])
clus1 #nomi cluster 1 #centrocampisti principalmente piu eleganti di qualita
predict(a.mclust)$z[c(clus1),]

vmedie1st=apply(datiStandardScaling[c(clus1),],2,mean) 
#ho preso i dati stndrdizz per cluster 1 e valuto media di ogni v.
#valuero per ciascun clus e le medie che si differeziano hanno inciso sul cluster maggiormente
#metodo forse non il piu corretto di tutti

vmedie1=apply(datiSoloVariabP90P100[c(clus1),],2,mean)#meglio valutando dati normalizz p90 o p100

clus2=names(a.mclust$classification[a.mclust$classification==2])
clus2 #nomi cluster 2 #difensori centrali forse piu agggressivi
predict(a.mclust)$z[c(clus2),]

vmedie2st=apply(datiStandardScaling[c(clus2),],2,mean)  #metodo non il piu corretto per vedere quali v piu importanti
vmedie2=apply(datiSoloVariabP90P100[c(clus2),],2,mean) 

clus3=names(a.mclust$classification[a.mclust$classification==3])
clus3 #nomi cluster 3 #attaccanti esterni 1vs1/quinti di centrocampo offensivi
predict(a.mclust)$z[c(clus3),]

vmedie3st=apply(datiStandardScaling[c(clus3),],2,mean) 
vmedie3=apply(datiSoloVariabP90P100[c(clus3),],2,mean)


clus4=names(a.mclust$classification[a.mclust$classification==4])
clus4 #nomi cluster 4 #mezzi trequartisti mezzale seconde punte
predict(a.mclust)$z[c(clus4),]
vmedie4st=apply(datiStandardScaling[c(clus4),],2,mean) 
vmedie4=apply(datiSoloVariabP90P100[c(clus4),],2,mean)

clus5=names(a.mclust$classification[a.mclust$classification==5])
clus5 #nomi cluster 5 #prime punte boa centrali
predict(a.mclust)$z[c(clus5),]
vmedie5st=apply(datiStandardScaling[c(clus5),],2,mean) 
vmedie5=apply(datiSoloVariabP90P100[c(clus5),],2,mean)

clus6=names(a.mclust$classification[a.mclust$classification==6])
clus6 #nomi cluster 6 #terzini di spinta forse tanti cross o conduttori di palla
predict(a.mclust)$z[c(clus6),]
vmedie6st=apply(datiStandardScaling[c(clus6),],2,mean) 
vmedie6=apply(datiSoloVariabP90P100[c(clus6),],2,mean)


clus7=names(a.mclust$classification[a.mclust$classification==7])
clus7 #nomi cluster 7 #centrocampisti di insrimmento corsa lotta 
predict(a.mclust)$z[c(clus7),]
vmedie7st=apply(datiStandardScaling[c(clus7),],2,mean) 
vmedie7=apply(datiSoloVariabP90P100[c(clus7),],2,mean)

#non ce cluster 8
clus8=names(a.mclust$classification[a.mclust$classification==8])
clus8 #nomi cluster 8 #dif centrali robusti piazzati sporchi di posizione non troppo veloci forsenon aggressivi
predict(a.mclust)$z[c(clus8),] 
vmedie8st=apply(datiStandardScaling[c(clus8),],2,mean) 
vmedie8=apply(datiSoloVariabP90P100[c(clus8),],2,mean)

#non ce cluster 9
clus9=names(a.mclust$classification[a.mclust$classification==9])
clus9 
vmedie9st=apply(datiStandardScaling[c(clus9),],2,mean) 
vmedie9=apply(datiSoloVariabP90P100[c(clus9),],2,mean)


#rbind(vmedie1,vmedie2,vmedie3,vmedie4,vmedie5,vmedie6)
#round(rbind(vmedie1,vmedie2,vmedie3,vmedie4,vmedie5,vmedie6),2) #6 cluster

ds_vmedie=round(rbind(vmedie1,vmedie2,vmedie3,vmedie4,vmedie5,vmedie6, vmedie7, vmedie8, vmedie9),2) #9 cluster
ds_vmedie
class(ds_vmedie)
t(ds_vmedie)

#provo a vedere con datiStandard come viene:
ds_vmediest=round(rbind(vmedie1st,vmedie2st,vmedie3st,vmedie4st,vmedie5st,vmedie6st, vmedie7st, vmedie8st, vmedie9st),2) #9 cluster
ds_vmediest
class(ds_vmediest)
t(ds_vmediest)

#confronto dif centr (clus2 e clus8)
t(ds_vmediest)[,c(2,8)]

t(ds_vmediest)[,c(2,8,9,1,4)] #confronto 9 terzini, 1 terzini/cc, 4 treq/quinti

t(ds_vmediest)[,c(4,3)] #treq/quinti  vs  attesterni

t(ds_vmediest)[,c(9,4,3,5)] #5 punte boa
t(ds_vmedie)[,c(9,4,3,5)]

t(ds_vmediest)[,c(2,8,9,1,6,7,4)]
t(ds_vmedie)[,c(2,8,9,1,6,7,4)]

t(ds_vmediest)[,c(1,6,7)]
t(ds_vmedie)[,c(1,6,7)]

#1 modo per trovare il rappr di un cluster:
colSums(abs(t(datiStandardScaling[c(clus1),]) - t(ds_vmediest)[,1])) 
which.min(colSums(abs(t(datiStandardScaling[c(clus1),]) - t(ds_vmediest)[,1])) )
cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[clus1,] #per vedere che squadre hanno quel tipo di giocatore
table(dati[c(clus1),2])
barplot(table(dati[c(clus1),2])) #2 è la v squadra #da sistemare colori, xlab

colSums(abs(t(datiStandardScaling[c(clus2),]) - t(ds_vmediest)[,2])) 
which.min(colSums(abs(t(datiStandardScaling[c(clus2),]) - t(ds_vmediest)[,2])) )
cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[clus2,] #per vedere che squadre hanno quel tipo di giocatore
table(dati[c(clus2),2])
barplot(table(dati[c(clus2),2])) #2 è la v squadra #da sistemare colori, xlab

dati$Squadra


colSums(abs(t(datiStandardScaling[c(clus3),]) - t(ds_vmediest)[,3])) 
which.min(colSums(abs(t(datiStandardScaling[c(clus3),]) - t(ds_vmediest)[,3])) )
cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[clus3,]
cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[c("Roberto Pereyra", "Sandi Lovrić" ,"Lazar Samardzic","Gerard Deulofeu" ,"Florian Thauvin"),]


cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[clus4,]

cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[clus5,]
table(dati[c(clus5),2])

cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[clus7,]
table(dati[c(clus7),2])


colSums(abs(t(datiStandardScaling[c(clus8),]) - t(ds_vmediest)[,8])) 
which.min(colSums(abs(t(datiStandardScaling[c(clus8),]) - t(ds_vmediest)[,8])) )
cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[c("Perr Schuurs","Rodrigo Becão","Jaka Bijol"),] #per vedere che squadre hanno quel tipo di giocatore
table(dati[c(clus8),2])
barplot(table(dati[c(clus8),2]))

colSums(abs(t(datiStandardScaling[c(clus9),]) - t(ds_vmediest)[,9])) 
which.min(colSums(abs(t(datiStandardScaling[c(clus9),]) - t(ds_vmediest)[,9])) )
cbind(dati$Squadra,dati$`MinutiGiocati/90 min`,round(datiSoloVariabP90P100,2))[clus9,]




# Installazione del pacchetto se non l'hai già
#install.packages("pheatmap")
# Carica il pacchetto
library(pheatmap)
pheatmap(t(ds_vmediest), color = colorRampPalette(c("blue", "white", "red"))(50))


# Elenco dei nuovi nomi di variabili estratti dal testo
nuovi_nomi_variabili <- c(
  "Contrasti", "Contrasti fuori dal terzo dif.", "Tiri bloccati","Intercetti",
  "Cartellini gialli","Falli commessi","Duelli aerei","Passaggi brevi","Palle lunghe",
  "Passaggi nel terzo off.","Pass. complet. in area di rig.","Cross in area di rig.",
  "Passaggi filtranti","Scambi in ampiezza","Cross","Tocchi in area avversaria",
  "Conduzioni progressive","Passaggi progressivi","Spazzate", "Dribbling", 
  "Tiri da fuori area", "Tiri da dentro area", "Tiri di piede", "Tiri di testa"
)

# Trasformazione della matrice in un dataframe lungo
matrice_long <- as.data.frame(as.table(ds_vmediest))
# Crea la heatmap con ggplot2
ggplot(matrice_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title="Heatmap",x = "Cluster", y = "Variabili", fill = "Scala Valori") +
  theme_minimal() +
  scale_y_discrete(labels = nuovi_nomi_variabili) +
  scale_x_discrete(labels =c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
                             "5-Finalizzatore","6-Equilibratore","7-Ruba Palloni","8-Marcatore","9-Laterale Creatore")) + #c("cluster1","cluster2","cluster3","cluster4","cluster5","cluster6",
  #"cluster7","cluster8","cluster9")
  theme(
        axis.text.x = element_text(angle = 20, hjust = 0.9, size = 17),  # Ingrandisci le etichette dell'asse x (cluster)
        axis.text.y = element_text(size = 15),  # Ingrandisci le etichette dell'asse y (variabili)
        axis.title.x = element_text(size = 16),  # Ingrandisci il titolo dell'asse x
        axis.title.y = element_text(size = 16),  # Ingrandisci il titolo dell'asse y
        plot.title = element_text(size = 25, hjust = 0.5, vjust = 1, face = "bold"),# Centra e ingrandisci il titolo
        panel.grid.major.x = element_line(color = "grey", size = 0.5),  # Aggiungi griglie verticali
        #panel.grid.major.y = element_line(color = "grey", size = 0.5) ,  # Aggiungi griglie orizzontali
        legend.title = element_text(size = 14)
        )



#composizione squadre
a.mclust$classification[dati$Squadra=="Napoli"] #squadra Napoli
table(a.mclust$classification[dati$Squadra=="Napoli"])
table(a.mclust$classification[dati$Squadra=="Napoli"])/length(a.mclust$classification[dati$Squadra=="Milan"] )

# Sample data
cluster_names <- c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
                   "5-Finalizzatore","6-Equilibratore","7-Ruba Palloni","8-Marcatore","9-Laterale Creatore")
proportionsNap <- c(20, 15, 20,0, 5,0,0,0,20)  # Replace with your data

# Create a pie chart with the color palette #chiedo a chatgpt lo stesso ma con ggplot
pie(proportionsNap, labels = cluster_names, main = "Proportions of Clusters Napoli", 
    col = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"))


# Calcolare le percentuali
percentages <- round(proportionsNap / sum(proportionsNap) * 100, 1)
labels <- paste(cluster_names, percentages, "%", sep = " ")

# Crea il grafico a torta
pie(proportionsNap, labels = labels, main = "Proportions of Clusters Napoli", 
    col = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"))


##
# Crea un grafico a torta senza etichette per i cluster con 0% di percentuale
# Filtra i cluster con percentuale 0
non_zero_clusters <- proportionsNap > 0
pie(proportionsNap[non_zero_clusters], 
    labels = labels[non_zero_clusters], 
    main = "Proportions of Clusters Napoli", 
    col = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink")[non_zero_clusters])


# Crea uno spazio più largo per la torta e la legenda con par()
par(mar = c(5, 4, 4, 8))  # Imposta i margini (top, right, bottom, left)

# Crea un grafico a torta senza etichette per i cluster con 0% di percentuale
# Filtra i cluster con percentuale 0
non_zero_clusters <- proportionsNap > 0

# Usa la funzione pie per creare il grafico
pie_chart <- pie(proportionsNap[non_zero_clusters], 
                 labels = "",  # Non mettere etichette automaticamente
                 main = "Proportions of Clusters Napoli", 
                 col = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink")[non_zero_clusters],
                 radius = 1, # Diminuisce la dimensione della torta per lasciare spazio alla legenda
                 cex = 1.2)  # Aumenta la dimensione del testo (per il titolo)




# Aggiungi la legenda con tutti i cluster, anche quelli con percentuale 0
legend("top", 
       legend = paste(cluster_names, percentages, "%", sep = " "), 
       fill = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"), 
       horiz = FALSE, 
       ncol = 1, 
       cex = 0.6, 
       bty = "n")
##



# Aggiungere la legenda in basso distribuita su 3 righe
legend("bottom", legend = paste(cluster_names, percentages, "%", sep = " "), 
       fill = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"), 
       horiz = FALSE, ncol = 3, cex = 0.8, bty = "n")

# Aggiungere la legenda
legend("topright", legend = paste(cluster_names, percentages, "%", sep = " "), 
       fill = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"), 
       cex = 0.8)



# Create a bar chart with the color palette
barplot(proportionsNap, names.arg = cluster_names, main = "Proportions of Clusters Napoli", 
        ylab = "Frequency", col = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"))

############################
# Load ggplot2 library
library(ggplot2)

# Prepare the data
cluster_names <- c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
                   "5-Finalizzatore", "6-Equilibratore", "7-Ruba Palloni", "8-Marcatore", "9-Laterale Creatore")
proportionsNap <- c(20, 15, 20, 0, 5, 0, 0, 0, 20)  # Replace with your data

# Create a data frame
dataNap <- data.frame(cluster = cluster_names, proportion = proportionsNap)

# Calculate percentages
dataNap$percentage <- dataNap$proportion / sum(dataNap$proportion) * 100

#
#install.packages("gridExtra")
library(gridExtra)

par(mfrow=c(1,2))
# Create a pie chart
gnap=ggplot(dataNap, aes(x = "", y = proportion, fill = cluster)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Proporzioni dei Cluster Napoli") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, color = "cyan", size = 16)) +  # Center and color the title
  scale_fill_manual(values = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink")) +  # Custom colors
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.7), 
            color = "white",
            size=3)  # Add percentage labels in white
gnap
########################

###
# NAPOLIII
#

# Prepare the data
cluster_names <- c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
                   "5-Finalizzatore", "6-Equilibratore", "7-Ruba Palloni", "8-Marcatore", "9-Laterale Creatore")
proportionsNap <- c(20, 15, 20, 0, 5, 0, 0, 0, 20)  # Replace with your data

# Create a data frame
dataNap <- data.frame(cluster = cluster_names, proportion = proportionsNap)

# Calculate percentages
dataNap$percentage <- dataNap$proportion / sum(dataNap$proportion) * 100

# Filter out clusters with 0% to avoid showing labels for those
dataNap_filtered <- dataNap[dataNap$proportion > 0, ]

# Install necessary package
# install.packages("gridExtra")
library(gridExtra)

# Create a pie chart with ggplot
gnap <- ggplot(dataNap, aes(x = "", y = proportion, fill = cluster)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Percentuali dei Cluster Napoli") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "cyan", size = 25, face = "bold"),  # Center and color the title
    legend.title = element_text(size = 15),  # Adjust legend title size
    legend.text = element_text(size = 10)   # Adjust legend text size
  ) +
  scale_fill_manual(
    values = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"),
    labels = paste0(cluster_names, " (", round(dataNap$percentage, 1), "%)")  # Add percentages to legend labels
  ) +
  # Add cluster names and percentages inside the pie chart with bold and outlined text
  geom_text(
    aes(label = ifelse(proportion > 0, paste0(cluster, "\n", round(percentage, 1), "%"), "")), 
    position = position_stack(vjust = 0.5),  # Position inside the slices
    color = "white", 
    fontface = "bold",  # Make the text bold
    size = 4.8,  # Increase the size of the text
    #stroke = 10,  # Add stroke to outline the text
    show.legend = FALSE  # Hide the text in the legend
  )

#
gnap+ theme(legend.text = element_text(size = 14))

###
# SAMPDORIAAAAAA
#


# Dati della Sampdoria
cluster_names_samp <- c("1-Regista", "2-Regista Arretrato", "3-Dribblatore", "4-Creatore di Occasioni",
                        "5-Finalizzatore", "6-Equilibratore", "7-Ruba Palloni", "8-Marcatore", "9-Laterale Creatore")

proportionsSamp <- c(0.04347826, 0, 0.04347826, 0, 0.17391304, 0.13043478, 0.21739130, 0.30434783, 0.08695652)  # Dati della Sampdoria

# Creazione del dataframe
dataSamp <- data.frame(cluster = cluster_names_samp, proportion = proportionsSamp)

# Calcolare le percentuali
dataSamp$percentage <- dataSamp$proportion / sum(dataSamp$proportion) * 100

# Caricare la libreria necessaria
library(ggplot2)

# Creare il grafico a torta con ggplot
gsamp <- ggplot(dataSamp, aes(x = "", y = proportion, fill = cluster)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Percentuali dei Cluster Sampdoria") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "blue", size = 25, face = "bold"),  # Centra e colora il titolo
    legend.title = element_text(size = 15),  # Modifica la dimensione del titolo della legenda
    legend.text = element_text(size = 10)   # Modifica la dimensione del testo della legenda
  ) +
  scale_fill_manual(
    values = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink"),
    labels = paste0(cluster_names_samp, " (", round(dataSamp$percentage, 1), "%)")  # Aggiungi le percentuali alle etichette della legenda
  ) +
  # Aggiungere i nomi dei cluster e le percentuali all'interno della torta per i cluster che non hanno 0%,
  # ma non mostrare "Dribblatore", "Regista" e "Laterale Creatore" negli spicchi della torta.
  geom_text(
    aes(label = ifelse(cluster %in% c("3-Dribblatore", "1-Regista", "9-Laterale Creatore") | proportion == 0, "", paste0(cluster, "\n", round(percentage, 1), "%"))), 
    position = position_stack(vjust = 0.5),  # Posizione all'interno degli spicchi
    color = "white", 
    fontface = "bold",  # Rende il testo in grassetto
    size = 4.8,  # Aumenta la dimensione del testo
    show.legend = FALSE  # Nasconde il testo dentro la torta nella legenda
  )

# Visualizzare il grafico
gsamp + theme(legend.text = element_text(size = 14))  # Aumenta la dimensione del testo della legenda


par(mfrow = c(2, 1))  # Due grafici uno sotto l'altro

# Primo grafico
print(gnap)
print(gsamp)

#sampdoria ultima in classifica:
(table(a.mclust$classification[dati$Squadra=="Sampdoria"]))
(table(a.mclust$classification[dati$Squadra=="Sampdoria"])/length(a.mclust$classification[dati$Squadra=="Sampdoria"]))
proportionsSamp=c(0.04347826,0, 0.04347826,0, 0.17391304, 0.13043478, 0.21739130, 0.30434783, 0.08695652)

proportionsSamp 

# Create a data frame
dataSamp <- data.frame(cluster = cluster_names, proportion = proportionsSamp)

# Calculate percentages
dataSamp$percentage <- dataSamp$proportion / sum(dataSamp$proportion) * 100

# Create a pie chart
#dataSamp <- dataSamp[dataSamp$proportion > 0, ]
gsamp=ggplot(dataSamp, aes(x = "", y = proportion, fill = cluster)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Proporzioni dei Cluster Sampdoria") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, color = "blue", size = 16)) +  # Center and color the title
  scale_fill_manual(values = c("red", "blue", "green", "orange", "purple", "cyan", "magenta", "black", "pink")) +  # Custom colors
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.75), 
            color = "white",
            size=3)  # Add percentage labels in white
gsamp

grid.arrange(gnap, gsamp, ncol = 2)
##

a.mclust$classification[dati$Squadra=="Lazio"] #squadra Lazio
table(a.mclust$classification[dati$Squadra=="Lazio"])

a.mclust$classification[dati$Squadra=="Milan"] #squadra Milan
table(a.mclust$classification[dati$Squadra=="Milan"])
table(a.mclust$classification[dati$Squadra=="Milan"])/length(a.mclust$classification[dati$Squadra=="Milan"] )

a.mclust$classification[dati$Squadra=="Atalanta"] #squadra Atalanta
table(a.mclust$classification[dati$Squadra=="Atalanta"])
table(a.mclust$classification[dati$Squadra=="Atalanta"]) / length(a.mclust$classification[dati$Squadra=="Atalanta"])

a.mclust$classification[dati$Squadra=="Inter"] #squadra Inter #mancano giocatori del cluster 7 e 8
table(a.mclust$classification[dati$Squadra=="Inter"])
table(a.mclust$classification[dati$Squadra=="Inter"]) / length(a.mclust$classification[dati$Squadra=="Inter"])

plot(table(a.mclust$classification[dati$Squadra=="Inter"]))


a.mclust$classification[dati$Squadra=="Juventus"] #squadra Milan
table(a.mclust$classification[dati$Squadra=="Juventus"])
table(a.mclust$classification[dati$Squadra=="Juventus"])/length(a.mclust$classification[dati$Squadra=="Milan"] )

a.mclust$classification[dati$Squadra=="Cremonese"] #squadra Milan
table(a.mclust$classification[dati$Squadra=="Cremonese"])
table(a.mclust$classification[dati$Squadra=="Cremonese"])/length(a.mclust$classification[dati$Squadra=="Milan"] )

table(a.mclust$classification[dati$Squadra=="Lecce"])

table(a.mclust$classification[dati$Squadra=="Sampdoria"])

table(a.mclust$classification[dati$Squadra=="Torino"])

table(a.mclust$classification[dati$Squadra=="Spezia"])

table(a.mclust$classification[dati$Squadra=="Hellas Verona"])
##########



# selez variabili
#install.packages("clustvarsel")
library(clustvarsel)
clustvarsel(datiStandardScaling)

#PROVO A CREARE SUBDTSET CO V SELEZIONATE DA SELEZIONE DI VARIABILI con clustvarsel
colnames(datiStandardScaling)
colnames(dati)
#mi da errore per v "P90 DribblingTent" NON CAPISCO PERCHE
head(datiStandardScaling[,c("P100DuelliAereiV+P", "P90TiriTesta", "P90Tiri dentro area", "P100Tiri Bloccati",
                            "P90Cross in area", "P90Cross", "P90Area off.","P90 DribblingTent",
                            "P100PrgC","P100CntrsFuoriDalTerzoDif", "P100Spazzate", "P90PPA", "P90PassFil")])
subsetSelezV=datiStandardScaling[,c(7,24,22,3,
                            12,15,16,20,
                            18,2,19,11,13)]                        
#c("P100DuelliAereiV+P", "P90TiriTesta", "P90Tiri dentro area", "P100Tiri Bloccati", 
                            #"P90Cross in area", "P90Cross", "P90Area off.", "P90 DribblingTent", 
                           # "P100PrgC", "P100CntrsFuoriDalTerzoDif", "P100Spazzate", "P90PPA", "P90PassFil")

head(subsetSelezV)
clustDopoSelezV=Mclust(subsetSelezV)
clustDopoSelezV
clustDopoSelezV$BIC
plot(clustDopoSelezV)

clustDopoSelezV$classification

predict.Mclust(clustDopoSelezV)$z

##
##
##
bgm_model=Mclust(datiRiduzDimUMAP$layout,G=13) #datiRiduzDimUMAP$layout qui ci sono le coordinate
bgm_model
#per controllare che non ci siano valori mancanti:  
any(is.na(dati))
any(is.na(datiRiduzDimUMAP$layout))

#esploraz risultati del mod:
summary(bgm_model)
plot(bgm_model) #poi scelgo quale vedere

#assegnaz delle etichette di clustering e prob di appartenenza:
cluster_labels=predict(bgm_model, uncertainty=TRUE) #vedo ohni u.s con quale prob appartiene ad ognuno dei 5 cluster
cluster_labels
#vettore che contien ela prob di apèartenenza al 1cluster per ciascuna u.s.
cluster1= cluster_labels$z[,1:2] 
cluster1 

(cluster_labels$z[,1] & cluster_labels$z[,2]) <0.67


which(cluster_labels$z[,1] >0.67)
which

class(cluster_labels$z)
pppp=as.data.frame(cluster_labels$z)
head(pppp)
bel=((pppp >0.33) & (pppp <0.67))
class(bel)
bello=as.data.frame(bel)

head(bello)


cluster_labels$z
ppp=cluster_labels$z
bel=((ppp >0.33) & (ppp <0.67))
bello=as.data.frame(bel)

#mi trova i nomi in cui ce prob tra 0.33 e 0.67 #cioe i giocatori ibridi
apply(bello, 1, function(x) any(x == TRUE)) #se ce TRUE vuol dire che in quella riga ce almeno 1 true, cioe una prob tra 0.33 e 0.67
index_righe_ibridi=which(apply(bello, 1, function(x) any(x == TRUE))) #ho i nomi e gli indici di TUTTI i giocatori ibridi

gioc_ibridi=cluster_labels$z[index_righe_ibridi,] 
gioc_ibridi_2cifreDecim=round(gioc_ibridi,2)

as.data.frame(gioc_ibridi_2cifreDecim)


#analisi dei cluster # usiamo datiSoloVariabP90P100 cosi possiamo interpretare i dati per 90min o per 100 tocchi
datiSoloVariabP90P100PiuSquad=cbind(dati$Squadra,round(datiSoloVariabP90P100,2))
analisi_cluster= data.frame(datiSoloVariabP90P100PiuSquad, Cluster=cluster_labels$classification ,Probabilita= round(cluster_labels$z,2))
analisi_cluster
round(analisi_cluster,2)

GiocClust1=analisi_cluster[analisi_cluster$Cluster==1,] #vedo i valori delle variabili e tutte le prob ai cluster per i giocatori apparteenti al cluster 1
analisi_cluster[analisi_cluster$Cluster==2,]
analisi_cluster[analisi_cluster$Cluster==3,]
analisi_cluster[analisi_cluster$Cluster==4,]
analisi_cluster[analisi_cluster$Cluster==5,]
analisi_cluster[analisi_cluster$Cluster==6,]
analisi_cluster[analisi_cluster$Cluster==7,]
analisi_cluster[analisi_cluster$Cluster==8,]
analisi_cluster[analisi_cluster$Cluster==9,]
analisi_cluster[analisi_cluster$Cluster==10,]
analisi_cluster[analisi_cluster$Cluster==11,]
analisi_cluster[analisi_cluster$Cluster==12,]
analisi_cluster[analisi_cluster$Cluster==13,]

probUSclust1=cluster_labels$z[analisi_cluster$Cluster==1,] #vedo solo tutte le prob ai cluster per i gioc appartenenti al cluster 1

#il seguente non funziona bene:
library(dplyr)
analisi_cluster %>% group_by(Cluster.classification) %>% summarise(media_v1=mean(datiSoloVariab$P100Cntrs), sd1=sd(datiSoloVariab$P100Cntrs),
                                                                   media_v2=mean(datiSoloVariab$`P100Cntrs Treq. off.`, sd2=sd(datiSoloVariab$`P100Cntrs Treq. off.`),
                                                                                 media_v3=mean(datiSoloVariab$`P100Tiri Bloccati`), sd3=sd(datiSoloVariab$`P100Tiri Bloccati`)))
ungroup()

#il seguente non funziona bene          
GiocClust1[mean(datiSoloVariab$P100Cntrs),sd(datiSoloVariab$P100Cntrs), mean(datiSoloVariab$`P100Cntrs Treq. off.`),sd(datiSoloVariab$`P100Cntrs Treq. off.`), mean(datiSoloVariab$`P100Tiri Bloccati`),sd(datiSoloVariab$`P100Tiri Bloccati`),
           mean(datiSoloVariab$P100Int), sd(datiSoloVariab$P100Int)]           
analisi_cluster[cluster_labels$classification==1,] #vedo le variabili originali per ciascun giocatore del cluster 1
#terzini offensivi  #hanno una piccola prob per cluster 3,4,10,11 ?

which(rownames(analisi_cluster)=="Theo Hernández")
analisi_cluster[1:2,]

analisi_cluster[rownames(analisi_cluster)=="Theo Hernández",] #classificato nel cluster 3
analisi_cluster[analisi_cluster$Cluster==1,] #un po terzini un po mezz ali

analisi_cluster[rownames(analisi_cluster)=="Leonardo Sernicola",]
analisi_cluster[analisi_cluster$Cluster==1,] 

#vediamo confronto con i terzini offensivi #cluster1 (piu dribbling) #dist prgrss maggiore 
analisi_cluster[analisi_cluster$Cluster==1,] 

#grafico con colori in base ai cluster
grafico=ggplot(data=dati_com_df, aes(x=V1,y=V2,label=nomi_righe, col=analisi_cluster$Cluster) )+
  geom_point()+
  geom_text(size=3,vjust=-0.5)+ #aggiungi le etichette dei nopmi dei giocatori
  labs(x="UMAP 1",y="UMAP 2")+ #eticheete degli assi
  theme_minimal()
grafico

#provo con pca o analisi correlazioni o altro

#come considero quei giocatori che hanno prob alte in piu cluster?
analisi_cluster[cluster_labels$z>0.30,] #righe prima per gioc ibridi

# ?
poppo=prcomp(datiStandardScaling,center = F)
poppo
poppo

#####################################

#PMM #POISON MIXTURE MODEL #V DI CONTEGGIO
install.packages("edgeR")
install.packages("HTSCluster")
library(HTSCluster)
install.packages("MBCbook")
library(MBCbook)
#non riesco a scaricare i oaccheti adatti

#######
#CON K-MEANS (valuto se kmeans si fa di dati originali o stndrdizzati)

#in qsto caso kmeans partendo da dati stndrdizzati
km13=kmeans(datiStandardScaling, centers = 13,nstart = 5)
km13
km13$size
km13$cluster
km13$centers

which(km13$cluster==6)
which(km13$cluster==8)

km13$totss  #2616  #dev tot = dev tra group + dev dentro i group

km13$withinss #tutte e 13 le dev dentro ciascun group
km13$tot.withinss #962.9271 #dev dentro i group #somma di tutte e 13 le singole dev dentro ciascun group

km13$betweenss #1653.073 # dev tra i group

km13$betweenss / km13$totss #0.63 # % dev tra group su dev tot (var spiegat su dev tot)

#explained.var=km13$betweenss / km13$totss
#plot(c(1), explained.var, lwd=2,xlab = "N.Cluster",ylab = "% var spiegata", type = "l")  


#SCREEPLOT 

#BIPLOT

#SILHOUETTE

##########
#####
# PCA 

varr=var(datiStandardScaling) #NON SI FA PCA DELL VAR O CORR, MA DEVO FAR var(datioriginali) per capire se le var sono sulla stessa scala
prcvarr=prcomp(varr)
summary(prcvarr) #prendo le prime 2 PC
prcvarr$rotation

correl= cor(datiStandardScaling)
prccorrl=prcomp(correl)
summary(prccorrl) #prendo le prime 2 PC
prccorrl$rotation

var(datiSoloVariab[,-1]) #vedo dalla diag princip che variabili non sono sulla stessa scala
var(datiSoloVariabP90P100) #qui v. normalizzate per P100 o p90
var(datiStandardScaling) #qui standrdzzate anche con met stndrd scaling

#############
prcstddavOriginali=prcomp(datiSoloVariab[,-1],scale. = T)
summary(prcstddavOriginali) #con standrdizz sua di R da 0 tengo solo 4 o 5 PC

############
prcstndrdizzDopoP90P100=prcomp(datiSoloVariabP90P100, scale. = T)
summary(prcstndrdizzDopoP90P100) #con stndrzz di R ma partendo gia da normalizz P90 o P100 è uguale alla successiva partendo dai dati gia belli stndrdizz a modo mio, cioe tengo le prime 6 PC

################
prc= prcomp(datiStandardScaling) #i miei dati qui erano gia belli stndrdizzati a modo mio
prc #le prime 6 hanno sd > 1
prc$sdev
summary(prc) 
##le prime 6 hanno sd > 1
# con 5 PC spiega il 71% var tot
# con 6 PC spiega il 76% var tot

prc$rotation
prc$x
#KMEANS DOPO PCA
datiUso6pc= prc$x[,1:6]
km13con6pc=kmeans(datiUso6pc, centers = 13,nstart = 5)
km13con6pc

km13con6pc$size
km13con6pc$cluster
km13con6pc$betweenss / km13con6pc$totss # 0.785566 ## % dev tra group su dev tot (var spiegat su dev tot)

which(km13con6pc$cluster==5) #theo dov e PCA+KMENAS
which(km13$cluster==6) #theo dov è SOLO KmeaNs

#UN PO DIVERSI

which(km13con6pc$cluster==6) #leao dov è PCA+KMENAS
which(km13$cluster==8) #lao dov è SOLO KmeaNs
#GIA PIU SIMILI


#per poi prendere i giocatori di un cluster e vedere le loro variabili facciamo:
indici_da_usare=which(km13$cluster==8)
datiStandardScalingPiu[indici_da_usare,]
