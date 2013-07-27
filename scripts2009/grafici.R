# 5. GRAFICI
#
###########################################################
#
# Funzioni grafiche di alto livello: plot()
#
# Esempio 1
# Diagramma della funzione seno (parametri grafici prestabiliti)
#
plot(sin, from=-2*pi,to=2*pi)
#
# Esempio 2
# Diagramma della funzione seno + parametri grafici di base
# xlab, ylab: descrittori degli assi coordinati
# main, sub: titolo e sottotitolo del grafico
# xlim, ylim: intervalli numerici degli assi coordinati
# lwd: spessore delle linee (valore prefissato 1)
# col: colore
#
plot(sin, from=-2*pi,to=2*pi, xlab="x", ylab="sen(x)",
main="Diagramma funzione seno in [-2pi, 2pi]", lwd=2, col="red")
#
# Esempio 3
# Diagramma della funzione f(x)=x^3-2x+1
#
plot(function(x) x^3-2*x+1, from=-10, to=10,
xlab="x", ylab="x^3-2x+1", main="Diagramma della funzione f(x) = x^3-2x+1")
#
# Esempio 4
# Sovrapponiamo al grafico precedente la retta di equazione y = 100x
#
plot(function(x) 100*x, from=-10, to=10, col="red", add=TRUE)
#
# Esempio 5
# Diagramma di dispersione della superficie(%) e della popolazione (%)
# delle regioni italiane
regio <- read.table(file="c:/corsoR/dati/regioni.txt", header=TRUE,
row.names=1, na.strings=":")
str(regio)
sup1 <- 100*regio$sup/sum(regio$sup)
pop1 <- 100*regio$pop02/sum(regio$pop02)
plot(sup1,pop1, type="p",
xlab="Superficie (% della superficie dell'Italia)",
ylab="Popolazione (% della popolazione italiana)",
main="Regioni italiane: superficie e popolazione")
#
# Esempio 6
# Miglioriamo la leggibilità del grafico precedente
# aggiungendo le sigle delle regioni e la bisettrice del primo quadrante
sigle <- c("PI","VA","LI","LO","TAA","VE","FVG","ER","TO","UM","MA","LA",
"AB","MO","CA","PU","BA","CAL","SI","SA")
text(sup1, pop1, labels=sigle, pos=4, cex=0.6)
plot(function(x) x, from=0, to=10, col="red", add=TRUE)
#
# Esempio 7
# Divisione della finestra grafica con layout()
# Diagrammi di dispersione dei dati USArrests
# Visualizzazione di descrittori delle unità statistiche (50 stati USA) con text() 
# e di variabili di stratificazione (state.division, area territoriale) con col()
#
?layout
layout(matrix(1:4,ncol=2))
str(USArrests)
str(state.abb) #sigle di due lettere dei 50 stati
str(state.division) #area territoriale (9 aree)
plot(USArrests[,c(1,2)],pch=20,col=gray(as.integer(state.division)/10),
xlab="Assassinio", ylab="Aggressione")
text(USArrests[,c(1,2)],labels=state.abb,cex=0.6,pos=4)
plot(USArrests[,c(1,4)],pch=20,col=gray(as.integer(state.division)/10),
xlab="Assassinio", ylab="Stupro")
text(USArrests[,c(1,4)],labels=state.abb,cex=0.6,pos=4)
plot(USArrests[,c(3,4)],pch=20,col=gray(as.integer(state.division)/10),
xlab="% Popolazione urbana", ylab="Stupro")
text(USArrests[,c(3,4)],labels=state.abb,cex=0.6,pos=4)
plot(state.x77[,3],USArrests[,1],pch=20,col=gray(as.integer(state.division)/10),
xlab="% Analfabeti", ylab="Assassinio")
text(state.x77[,3],USArrests[,1],labels=state.abb,cex=0.6,pos=4)   
#
# Esempio 8
# Diagramma di serie temporali
# I dati sono i valori della piena del Nilo misurati annualmente dal 1871 al 1970
# Dati "Nile" della libreria di R (oggetto di classe time-series)
str(Nile)
Nile
plot(Nile, xlab="Anni", ylab="Livello del Nilo",
main="Andamento della piena del Nilo ad Assuan, 1871-1970")
#
# Esempio 9
# Diagramma di una serie temporale multivariata
# Variabili: superficie dello Stato italiano (kmq), popolazione residente, densità
# Dati rilevati nei censimenti dal 1861 in poi
# Manca il dato di popolazione del 1891
# ts() crea un oggetto di classe "time series"
?ts 
sup <- c(248032, 285930, 285948, 285948, 285948, 285948, 310144, 310079,
310190, 301201, 301224, 301252, 301263, 301302, 301338)
pop <- c(22212000, 27303000, 28953480, NA, 32965504, 35845048, 38449000,
41651617, 42993602, 47515537, 50623569, 54134846, 56556911, 56778031, 56995744)
cens <- ts(cbind(sup/1000,pop/1000000,pop/sup), start=1861, frequency=1/10,
names=c("Sup. x 1000 kmq", "Popol. Milioni", "Densità"))
plot(cens, type="b",xlab="Anni",
main="Italia: superficie, popolazione, densità 1861-2001")
#
###########################################################
#
# Funzioni grafiche secondarie (primitive grafiche)
# points(x, y): punti con coordinate specificate nei vettori x, y
# lines(x, y): linea congiungente i punti di coordinate x, y
# abline(a, b), abline(h), abline(v): retta descritta dall'equazione cartesiana 
# polygon(x, y): poligono con vertici di coordinate x, y
# text(x, y, labels=...): stringhe di testo descritte da labels 
# nelle posizioni specificate dai punti di coordinate x, y
# legend: legenda
#
# Esempio 1
# Campione casuale (distribuzione uniforme) di punti del quadrato unitario
#
?abline
plot(runif(1),type="n", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="",
main="1000 punti a caso dalla distr. Uniforme in [0,1]x[0,1]")
abline(h=seq(0,1,by=0.1),lty="dotted",col="red")
abline(v=seq(0,1,by=0.1),lty="dotted",col="red")
points(runif(1000),runif(1000),pch=20) 
#
# Esempio 2
# Cerchi "a caso" sul quadrato unitario
#
plot(runif(1),type="n",xlim=c(0,1),ylim=c(0,1),xlab="", ylab="",main="")
points(runif(20), runif(20), pch=19, cex=10*runif(20), col=gray(runif(20)))
#
###############################################################################

              
 
  
