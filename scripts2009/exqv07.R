# DATA MINING E SISTEMI INFORMATIVI
# QUALITA' DELLA VITA 2007
# FONTE DEI DATI : IL SOLE-24ORE (www.ilsole24ore.com)
###############################################################################
# Il file qual07.csv contiene i dati dell'indagine QUALITA' DELLA VITA 2007
#  de Il Sole-24 Ore. 
#  Le unità di rilevazione sono le province italiane. Le variabili sono divise
#  in 5 gruppi: A, tenore di vita, B: affari e lavoro, C: ambiente,
#  D: criminalità, E: demografia, F: tempo libero. Ci sono inoltre due variabili
#  qualitative: prov (nome della provincia), Area  (area territoriale, 5 modalità:
#  NO, NE, C, S, I).
#  Estraete i dati delle variabili A e B, tralasciando A_ici.
#  Studiate le distribuzioni marginali e congiunta di A_depb e B_int;
#  ci sono differenze importanti tra le aree territoriali?
#  quanto vale la correlazione lineare tra le due variabili?
#  in quale provincia si rileva il massimo tasso d'interesse? 
#  Calcolate la matrice di correlazione; quali sono le coppie di variabili più/
#  meno correlate? Qual è il rango della matrice?
#  Eseguite l'analisi delle componenti principali. Quante componenti sono
#  necessarie per spiegare il 70%, almeno, della varianza totale?
#  Come interpretate le prime due componenti? In quali province si osservano
#  il minimo e massimo valore della prima componente? La distribuzione delle
#  province è omogenea o è una mistura di gruppi ben separati? Verificate
#  empiricamente che le prime due componenti sono incorrelate. 
###############################################################################
# Descrizione di massima delle variabili
#   vagg valore aggiunto pro capite
#   depb depositi bancari pro capite
#   pens importo medio pensione INPS
#   benidur spesa media per acquisto di beni durevoli 
#   percimpr numero imprese ogni 100 abitanti
#   natimortimpr indice di natimortalità delle imprese
#   disocc tasso di disoccupazione
#   occgio tasso di occupazione giovani
#   int tasso d'interesse
#   protesti  protesti bancari
#   Maggiori dettagli alla pagina web dell'indagine.
#   prezzomq prezzo medio mq appartamenti semicentrali 
###############################################################################
#INPUT DATI
qv <- read.csv2("http://venus.unive.it/romanaz/datamin/dati/qual07.csv",
 header=TRUE)
###############################################################################

