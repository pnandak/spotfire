# 1. PRIMI PASSI
#
###########################################################
#
# Assistenza: help(funzione/oggetto) oppure ?funzione/oggetto
# Visualizza informazioni sulla funzione o sull'oggetto specificati
#
# L'esempio mostra le informazioni sulla costante R "pi" (pi greco)
#
help(pi)
pi
#
# Il secondo esempio riguarda la codifica della data
#
?date
date
#
# Assistenza: apertura di un ipertesto riguardante tutto il sistema
# con la funzione help.start()
help.start()
#
Assistenza: lista dei packages compresi nell'installazione con library()
#
library()
#
# Assistenza: caricamento di un package con library(nome package)
# Ad esempio per caricare il package class (cluster analysis)
#
library(class)
#
###########################################################
#
# Librerie di dati in linea: funzione data()
#
data()
#
# Dati "islands", superficie delle grandi masse terrestri
# 
?islands
islands
#
# Dati "state.x77", variabili socio-demografiche rilevate sui 50 stati USA
?state.x77
state.x77
#
############################################################################  
#
# Campioni di dati pseudo-casuali
#
runif(n=10) #campione di 10 elementi dalla distribuzione uniforme R(0, 1)
rnorm(n=20, mean=0,sd=1) #campione di 20 elementi dalla normale standard N(0, 1) 
rbinom(n=20,size=1,prob=0.5) #20 lanci di una moneta regolare, 0: croce, 1: testa 
#              
############################################################################  
#
# Ordinamento di dati: funzione sort()
#
sort(rbinom(n=20,size=1,prob=0.5)) #ordinamento di dati numerici
sort(c(letters,LETTERS)) #ordinamento di dati alfanumerici
#
############################################################################  
#
# Tempo richiesto dall'esecuzione di un comando: system.time()
#
help(system.time)
system.time(sort(runif(n=1000000)))
#
############################################################################  
#
# Operatori aritmetici: +, -, *, /, ^
#
?"+"
2*(5-3)^2/8
2*pi*10; pi*10^2 #Cerchio di raggio 10: lunghezza della circonferenza e area del cerchio
(1+0.015)^10 #Capitale maturato da 1 euro prestato per 10 anni al tasso dell'1.5% (interesse composto)                
#
# Operatore di assegnazione: "<-","->"
#
a <- 100; 2*pi -> b #le costanti 100 e 2pi sono registrate permanentemente in memoria
a
b
camp_norm <- rnorm(10)
camp_norm
#
# Operatori di confronto binario: <, <=, >, >=, ==, !=
#
a > 0
b > a
pi != 3.141593 #perchè questo risultato?
2*pi == b
0.4 + 0.1 == 0.6 - 0.1 #perchè questo risultato?
camp_norm > 0 #l'operatore è applicato dato per dato
"ab" == "AB" #confronto di dati alfanumerici
#
# Operatori logici: ! (NOT), & (AND), | (OR)
#
pi >= 3 & pi <=4
camp_norm < -3 | camp_norm > 3 #ci sono dati esterni all'intervallo (-3, 3)?
#
# Elenco e/o rimozione degli oggetti memorizzati: ls(), rm()
#
ls()
rm(a,b)
ls()
#
# Funzioni matematiche elementari
#
abs(camp_norm) #valore assoluto, opera dato per dato
sqrt(pi) #radice quadrata
log(pi) #logaritmo naturale
log10(100) #logaritmo in base 10
exp(1) #esponenziale
exp(-1/2)/sqrt(2*pi) #valore della densità normale standard in x=1 (o -1)
sin(pi/4); cos(pi/2); atan(1) #funzioni trigonometriche
factorial(4) #fattoriale
choose(10,3) #coefficiente binomiale
#
# Paesaggi esotici: -Inf, Inf, NaN (non è un numero), NA (non disponibile, mancante)
#
?Inf
10/0
10/0 - 1/0
sqrt(camp_norm)
log(camp_norm)
as.numeric(" ") # la conversione in numero dello spazio bianco è "dato mancante", non zero
#
#########################################################################

  
