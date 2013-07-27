# 3. ALTRE STRUTTURE: MATRICE, DATA FRAME, FATTORE

###########################################################

# Matrice: insieme di vettori (righe o colonne)
# Non sono ammesse modalità eterogenee
###########################################################
#
# Costruzione di matrici con matrix()
#
m1 <- matrix(1:6,nrow=3,ncol=2,byrow=FALSE) #numeri da 1 a 6 in due colonne
m1
m2 <- matrix(1:9,nrow=3,ncol=3,byrow=TRUE) #numeri da 1 a 9 in tre righe
m2
###########################################################
#
# Costruzione o modifica di matrici con rbind(), cbind()
# rbind() allinea righe
# cbind() affianca colonne
#
rbind(m1,c(1,1))                        #aggiunge una quarta riga di 1
cbind(c(1,0,1),m1)                      #aggiunge una prima colonna
cbind(c(1,0,0),c(0,1,0),c(0,0,1))       #matrice identica d'ordine 3
###########################################################
#
# Le matrici possono essere logiche o alfanumeriche
#
matrix(as.character(m1),3,2)
m1 > 2
##########################################################
#
# Estrazione di submatrici
#
m1[3,1]                                 #elemento di posto 3,1
m2[1,]; m2[,2]                          #prima riga, seconda colonna
m2[1:2,1:2]                             #submatrice quadrata d'ordine 2
####################################################################
#
# Trasformazione di matrici: operatori aritmetici (valgono le regole dei vettori)
#
m3 <- matrix(1:4,2,2); m4 <- matrix(c(1,0,0,1),2,2)
m3; m4
2*m3; m3+1                              #trasformazioni elemento per elemento
m3+m4; m3*m4                            #somma(moltiplica) elementi corrispondenti
#######################################################################
#
# Funzioni matriciali
#
t(m1)                                   #trasposta
solve(m3)                               #inversa
diag(m3)                                #estrae il vettore degli elementi diagonali
diag(rep(1,3))                          #costruisce una matrice diagonale da un vettore
rowSums(m1)                             #somme di riga
colSums(m3)                             #somme di colonna
sum(diag(m3))                           #traccia
det(m2)                                 #determinante
m2%*%m1                                 #prodotto "righe per colonne", le matrici devono essere conformate
m1%*%m2                                 #messaggio d'errore 
#
###########################################################
#
# Fattore: vettore usato per descrivere la tipologia degli elementi
# di altri vettori di uguale lunghezza
# Corrisponde spesso a variabili di classificazione o stratificazione
#
# Costruzione di fattori con factor()
#
# Presidenti americani 1897-
pres <- c("W. McKinley", "T. Roosevelt", "W. H. Taft", "W. Wilson",
"W. G. Harding", "C. Coolidge", "H. C. Hoover", "F. D. Roosevelt",
"H. S. Truman", "D. D.Eisenhower", "J. F. Kennedy", "L. B. Johnson",
"R. Nixon", "G. R. Ford", "J. E. Carter", "R. Reagan", "G. Bush", 
"B. Clinton", "G. W. Bush Jr.")
# Durata della presidenza, anni
anni <- c(4, 8, 4, 8, 4, 6, 4, 12, 8, 8, 2, 6, 5, 3, 4, 8, 4, 8, 4)
# Partito, D: democratico, R: repubblicano
partito <- factor(c("R", "R", "R", "D", "R", "R", "R", "D", "D", "R", "D", "D",
"R", "R", "D", "R", "R", "D","R"), ordered=FALSE)
#
# Applicazione di funzioni ai gruppi definiti da un fattore, tapply()
#   
tapply(pres, partito, length)
tapply(anni, partito, sum)
#
############################################################
#
# Data frame: tabella di dati ordinati in righe e colonne
# Vettori colonna possono avere modalità diverse
# Corrisponde alla tabella unità x variabili frequentemente usata in Statistica
################################################################
#
# Costruzione di data frame con data.frame()
#
nome <- c("Piemonte", "Valle d'Aosta", "Liguria", "Lombardia",
"Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia",
"Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo",
"Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
 "Sardegna")
# Nomi delle regioni
# 
area <- rep(c("N-O", "N-E", "Centro", "Sud", "Isole"),c(4,4,4,6,2))
# Area territoriale (variabile di classificazione)
#
sup <- c(25399, 3263, 5421, 23861, 13607, 18379, 7844, 22123, 22997,
8456, 9694, 17208, 10799, 4438, 13595, 19363, 9992, 15080, 25707, 24089)
# Superficie (kmq)
#
costa <- c(NA, NA, 346, NA, NA, 156, 110, 130, 573, NA, 172, 357, 124, 34,
461, 830, 59, 710, 1425, 1636)
# Sviluppo costiero (km, NA: dato mancante)
#
pop <- c(4231334, 120909, 1572197, 9108645, 950495, 4577408, 1191588,
4030220, 3516296, 834210, 1484601, 5145805, 1273284, 321047, 5725098, 
4023957, 596821, 2007392, 4972124, 1637639)
# Popolazione residente 1/1/2003
#
regioni <- data.frame(nome, area, sup, costa, pop, row.names="nome")
str(regioni)              #struttura del data frame
dim(regioni)              #dimensioni del data frame
#####################################################################
#
# Aggiungere variabili (ad esempio la densità di popolazione)
#
regioni$dens <- regioni$pop/regioni$sup
str(regioni)
#################################################################
#
# Sottoinsiemi del data frame con l'operatore []
#
# Regioni del nord
regioni[regioni$area == "N-O" | regioni$area == "N-E",]
# oppure
regioni[1:8,]
# Estrai le variabili pop e dens
regioni[, c(4,5)]
# Estrai lo sviluppo costiero per le regioni con sbocco sul mare
regioni[!is.na(regioni$costa), 3]
##################################################################
#
# Applicazione di funzioni e trasformazioni
sum(regioni[,2])    # superficie totale
max(regioni[,5])    # massima densità
##################################################################
#
# La variabile area è interpretata come un fattore 
tapply(regioni$pop, regioni$area, length)
tapply(regioni$pop, regioni$area, sum)
tapply(regioni$dens, regioni$area, mean)
#
################################################################
#
# Funzione:    
#
# Costruzione della funzione che calcola la somma dei primi n numeri naturali
# n >= 1
somma_n <- function(n) n*(n+1)/2
somma_n(10)
somma_n(1)
#
# Costruzione della funzione che calcola la distanza euclidea di due punti
# (o vettori) dello spazio euclideo p-dimensionale (p >= 1)
d_eucl <- function(a, b) sqrt(sum((a-b)^2))
d_eucl(c(1,0,0), c(0,1,0))
a <- c(0,1,0); d_eucl(a,a)
#
################################################################


  



  