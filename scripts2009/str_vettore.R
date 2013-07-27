# 2. UNA STRUTTURA BASE: VETTORE

##########################################################
# Vettore: insieme ordinato di dati con la stessa modalità
# (numeri, caratteri alfanumerici, costanti logiche)
##########################################################
#
# Costruzione di vettori con c(), concatenate
#
v1 <- c(-1, 0, 2, 5, 9)
v1  
str(v1)
mode(v1)
length(v1)
c(v1,pi)                                        #pi: pi greco
#############################################################
#
# Costruzione di vettori con rep(), repeat
#
rep(2,times=4)
rep(2,4)                                        #forma breve     
rep(c(0,1),3)
rep(c(0,1),c(3,2))
##############################################################
#
# Costruzione di vettori con seq(), sequence
#
seq(from=0,to=10,by=2)
seq(from=0,to=10,length=3)
seq(0,10,by=3)                                  #forma breve
###############################################################
#
# Operatore :
#
1:6
0.5:4
###############################################################
#
# Vettori alfanumerici
#
v2 <- c("lun","mar","mer")
v2
mode(v2)
rep(c("a","b"),c(2,4))
LETTERS                                         #lettere maiuscole       
letters                                         #lettere minuscole
#####################################################################
#
# Vettori logici
#
v3 <- c(TRUE,TRUE,FALSE)
v3
mode(v3)
v4 <- rep(c(0,1),c(2,3))
v4
v4 == 0                                        #vettore logico identifica componenti nulle di v4
#
####################################################
#
# Trasformazione di vettori: operatori aritmetici
# 
v1 <- 1:5; 2*v1-1                              #trasformazione componente per componente
v2 <- c(1,1,1,0,0); v1+v2                      #somma componenti corrispondenti
v3 <- -1:1; v2+v3                              #attenzione! vettori di lunghezza diversa, il vettore più corto viene riciclato
########################################################################
#
# Trasformazione di vettori: funzioni elementari
#
v4 <- c(0,6,10,4)
v4^2   
sqrt(v4)
#########################################################################
#
# Ordinamento
#
sort(v4)                                       #ordina in senso crescente
sort(v4,decreasing=TRUE)                          #ordina in senso decrescente
sort(v4,index.return=TRUE)                        #fornisce la permutazione
c(min(v4),max(v4))                             #minimo, massimo
c(which.min(v4),which.max(v4))                 #indici del minimo e del massimo
sort(c("Bruno","Mario","Luisa","Fabio","Valeria","Marco")) #anche dati alfanumericive 
##############################################################################
#
# Conversione di modalità
#
as.character(c(-1,0,10))         #ogni modalità può essere convertita in carattere alfanumerico
as.numeric(c("a","1","a1","1))   #qual è la regola? (NA, not available, dato mancante)

####################################################

# Selezione di sottoinsiemi: operatore []

vett <- -3:3
vett[3]                          #terzo elemento
vett[length(vett)]               #ultimo elemento
vett[2:5]                        #dal secondo al quinto
vett[vett>0]                     #elementi positivi
vett[vett < -2 | vett > 2]       #elementi esterni all'intervallo chiuso [-2, 2]

 