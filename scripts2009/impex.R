# 4. IMPORTAZIONE - ESPORTAZIONE DI DATI
#
###########################################################
#
# Importazione dati: funzione read.table()
# Legge tabelle in cui tutte le righe hanno lo stesso numero di colonne
# La prima riga può contenere i nomi delle colonne
# Costruisce un oggetto con modalità data.frame
#
help(read.table)
regioni <- read.table(file="c:/corsoR/dati/regioni.txt",header=TRUE,
row.names="nome",as.is="capo",na.strings=":")
str(regioni)
regioni
############################################################
#
# Importazione dati: funzione scan()
# Costruisce vettori o liste
#
# Vettore dei nomi dei mesi
mesi <- scan(file="c:/corsoR/dati/mesi.txt", what="")
str(mesi)
#
# Nel seguente esempio scan() costruisce un vettore alfanumerico in cui
# ogni componente è un verso della poesia "Soldati" di Ungaretti (1918)
# Separatore di campo "\n" (a capo)
soldati <- scan(file="c:/corsoR/dati/soldati.txt", what="", sep="\n")
#
# E' semplice costruire matrici con i dati letti da scan()
# Il seguente esempio costruisce la matrice identica d'ordine 4
matrix(scan(file="c:/corsoR/dati/mid4x4.txt", what=0),ncol=4)
#
# Nel seguente esempio scan() legge una lista i cui elementi sono le Venezie
# (Trentino-Alto Adige, Veneto, Friuli-Venezia Giulia)
# Le variabili sono il nome della regione, il capoluogo,
# il numero delle province, l'elenco delle province
venezie <- scan(file="c:/corsoR/dati/venezie.txt", what=list(nome="",
capoluogo="",numero_province=0,elenco_province=""))
#
###########################################################
#
# Esportazione dati: funzione write.table()
#
?write.table
# Scriviamo sul file "giorni.txt" i nomi dei giorni della settimana
#
write.table(c("Lunedì","Martedì","Mercoledì","Giovedì","Venerdì",
"Sabato","Domenica"),file="c:/corsoR/dati/giorni.txt",quote=FALSE,
row.names=FALSE,col.names=FALSE)
#
# Scriviamo sul file "matr4x2.txt" la matrice avente nelle colonne i numeri
# da 1 a 8
#
write.table(matrix(1:8,ncol=2,byrow=FALSE),file="c:/corsoR/dati/matr4x2.txt",
row.names=FALSE,col.names=FALSE)
#
# Il data set R "USArrests" è un data frame contenente per i 50 stati USA
# le seguenti variabili
# Murder: numero di arresti per assassinio per 100000 abitanti
# Assault: numero di arresti per aggressione per 100000 abitanti
# Rape: numero di arresti per stupro per 100000 abitanti
# UrbanPop: popolazione urbana (%)
# Nell'esempio seguente esportiamo i dati nel file arrestiUSA.txt
write.table(USArrests,file="c:/corsoR/dati/arrestiUSA.txt")     
#
# Il data set R "eurodist" contiene le distanze stradali in km tra 21 città europee
# Nell'esempio seguente esportiamo i dati nel file distanze_città.txt
# E' necessaria la trasformazione as.matrix(eurodist) perchè eurodist non è un data frame
write.table(as.matrix(eurodist),file="c:/corsoR/dati/dist_citta.txt")
#
############################################################################  


              
 
  
