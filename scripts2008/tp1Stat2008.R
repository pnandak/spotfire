#Préliminaire

sample(1:6,10,replace=T)

plot(sample(c(0,1), 10, replace=T), ylim=c(-1,2), type="o")

#source("tp1.R")

#q("no")

#2 Les objets R
#2.1 Création de vecteurs

#2.1.1 Création de vecteurs numériques
vec1 = c(2.8, 2.4, 2.1, 3.6, 2.8)
vec1


#2.1.2 Création de vecteurs chaînes de caractères
vec2 = c("rouge", "vert", "vert", "vert", "jaune")
vec2

#2.1.3 Création de vecteurs logiques
vec3 =  c(T,T,F,F,F)
vec3

#2.1.4 Création par répétition
 rep(4,3)

vec4 = rep(vec1,2)
vec4

vec5 = rep(vec1,c(2,1,3,3,2)
vec5

#2.1.5 Création par construction de suites

vec6 = 1:10
vec6

vec7 = seq(from = 3, to = 5,by = 0.2)
vec7

vec7 = seq(from = 3, length =11, by = 0.2)

#2.1.6 Création composante par composante

vec8 = numeric()
vec8[1] = 41.8
vec8[2] = -0.3
vec8[3] = 92
vec8

vec9 = character()

vec10 = logical()

vec8 = numeric(3)

#2.2 Création de matrices

#2.2.1 Méthode générique
mat1 =  matrix(vec4, ncol = 5)
mat1

dim(mat1) = c(2,5)

mat2 = matrix(vec4, ncol = 5, byrow = T)
mat2

#2.2.2 Autre méthode
mat3 = cbind(vec1,3:7)
mat3

#2.2.3 Extraire une sous-matrice, ou  une colonne, ou une ligne
mat1[,c(2,4,5)]

mat1[,c(F,T,F,T,T)]

mat3[c(1,4),]

mat3[c(T,F,F,T,F),]

mat1[,3] 

#2.2.4 Calculer les dimensions d'une matrice

dim(mat1)

#2.3 Création de listes

#2.3.1 Méthode directe:
list1 = list(vec1,c("rouge","bleu"), mat1)
list1

#2.3.2 Méthode composante par composante:
list1 = list()
list1[[1]] = vec1
list1[[2]] = c("rouge","bleu")
list1[[3]] =  mat1

#2.4 Opérations sur les vecteurs

#2.4.1 Copier, rallonger, extraire, raccourcir,
#      remplacer, nommer les élements, connaître
#      et changer la nature

#2.4.1.1 Copier
vec11 = vec1
vec13 =  vec12 =  vec11

#2.4.1.2 Rallonger
c(vec1, c(3.9, 2.7))

c("blanc", vec2)

#2.4.1.3 Extraire
vec1[2]
vec1[c(2,4)]
vec1[2:4]
vec1[vec3] 

#2.4.1.4 Raccourcir
vec11[-c(2)]
vec12[-c(2,4)]
vec13[-(2:4)]

#2.4.1.5 Remplacer
vec5
vec5[3:5] = c(1034, 238, -99)
vec5

#2.4.1.6 Nommer les élements
names(vec1) = c("julie","paul","solveigh","valentin","elsa")

vec1["paul"] 

#2.4.1.7 Connaître et changer la nature
mode(vec5)

is.numeric(vec5)

vec14 = as.character(vec4)
vec14

#2.4.2 Opérations arithmétiques sur des vecteurs numériques

#2.4.2.1 Opérations simples
vec1 + 4

2 * vec1 - c(1,2)

#2.4.2.2 Application d'une fonction:
#        vecteur longueur k -> vecteur longueur k
sin( vec1)

round(c(9.238, -1.34222), 2)

sort(vec1)

order(vec1)

#2.4.2.3 Application d'une fonction:
#        vecteur longueur k -> une valeur numérique
mean(vec1)

#2.4.2.4 Application d'une fonction: 
#        vecteur longueur k -> vecteur de longueur 2
range(vec1)

#2.4.2.5 Autres applications

#2.4.3 Opérations sur les vecteurs chaînes de caractères

#2.4.3.1 Coller deux chaînes
paste("jules","jim")

paste("jules","jim",sep="")

#2.4.3.2 Coller deux (ou plus) vecteurs de chaînes de caractères
paste("X",1:4,sep="")

#2.4.4 Opération sur les vecteurs logiques

#2.4.4.1 Opérateurs de comparaison}
c(1,4, -2, 5) < c(2,4,-3,6)

a = c(3,2,8,3,-3)
b = c(1,2)
a <= b

a > 2.5

vec1[ vec1 > 2.5 ]

#2.4.4.2 Opérateurs appliqués à des vecteurs logiques
a = c(3,2,8,3,-3)
b = c(1,2)
any( a <= b)

all( a <= b)

#2.5 Opérations sur les listes

#2.5.1 Nommer les élements d'une liste

names(list1) = c ("poids","couleur","matrice")

list2 = list(poids = vec1, couleur = c("rouge","bleu"), matrice = mat1)

list1 

list2

#2.5.2 Extraire un élement d'une liste
list1$couleur

list1[[2]]


#2.6 Opérations sur les matrices

#2.6.1 Nommer les lignes et colonnes
dimnames(mat1) =  list(c("jules","jim"), paste("X",1:5,sep=""))
mat1

mat1["jim","X3"]

#2.6.2 Opérations arithmétiques
mat1 + 3

#2.6.3 Opérations spécifiques

#2.6.3.1 Produit de matrices:

#2.6.3.2 Transposé d'une  matrice:

#2.7 Valeurs manquantes ou absence de données

dimnames(mat1) = list(NULL, paste("X",1:5,sep=""))

vec14 =  c(31,43,NA,33)

is.na(vec14)


#2.8 Structure de données: data.frame et objet ts

#2.8.1 Construction
dat1 = as.data.frame(mat1)

age = c(24,26,22)
sexe = c("H","H","F")
love = c(T,F,T)
enquete = data.frame(age,sexe,love)

sexe = factor(c("H","H","F"))

levels(sexe)

names(enquete) = c("Age","Sexe","Love")

row.names(enquete) = c("Jules","Jim","Elsa")

enquete = data.frame(Age = c(24,26,22), Sexe = c("H","H","F"), Love = c(T,F,T))

#2.8.2 Travail avec une data.frame
enquete[1:2,2:3]

attach(enquete)

Age

detach(enquete)


#2.8.3 Construction de tableaux de
#      contingence avec une data.frame
attach(enquete)

table(Sexe)

table(Sexe, Love)

#2.8.4 Construction d'un objet ts (times series)
library(ts)

ts(c(1,3,2,4,4,3,5,2,3,4,1,8,1,3,2,4,5,2,3,2,2,2,4,3), start = c(2002,3), freq = 12)

ts(c(1,3,2,4,4,3,5,2,3,4,1,8,1,3,2,4,5,2,3,2,2,2,4,3), start = c(2002,3), freq = 4)


#2.9 Lire ou écrire des données à
#    l'extérieur d'un fichier de commande R

#2.9.1 Lire des données

#2.9.1.1 La fonction scan.

#vec15 = scan("donnees.txt")'\\

donnees = matrix(scan("donnees.txt"), ncol = 3, byrow=T)

#2.9.1.2 La fonction read.table

#donnees = read.table("donnees.txt")

#donnees = read.table("donnees.txt", header = T)

#2.9.2 Ecrire des matrices ou des data.frame obtenues avec  R dans un
#      fichier texte
  


#2.10 Les  outils pour créer des fonctions

#2.10.1 Les instructions conditionnelles

#2.10.1.1 if et else

#2.10.1.2 for, while, repeat et break
a = numeric()
for(i in 1:5) a[i] = sin(vec1[i])

sin(vec1)

#2.10.2 Construire ses propres fonctions
arrangement = function(n, k) choose(n, k)*gamma(k+1)

arrangement(7,3)

moyennemat = function(x) {
  a = numeric()
  nbcolonne = dim(x)[2]
  for (i in 1:nbcolonne) a[i] = mean(x[,i])
  return(a)
}

moyennemat(mat1)'

b = moyennemat(mat1)'

moyennemat

#source("ecritfonction.R")

imagealeatoire = function() plot(runif(30),runif(30))
imagealeatoire()

compter = function(a,b) {
 d = numeric()
 for(i in 1:length(a))
  d[i] = sum(b==a[i])
 names(d) = as.character(a)
 return(d)
}


couleur = c("rose","vert","jaune")

fleurs = c("rose","rose","vert","rose","rose","jaune","jaune")

compter(couleur, fleurs)


#2.10.3 Fonctions s'appliquant à des objets plus complexes

#2.10.3.1 Application d'une fonction aux lignes (resp. colonnes)
#         d'une matrice: la fonction apply
apply(mat1, 1, mean)

apply(mat1, 2, mean)


#2.10.3.2 Application d'une fonction aux élements
#         d'une liste: les fonctions sapply et lapply
liste = list(c(1,2),c(3,1,2))
sapply(liste, mean)

lapply(liste, sd)


#2.10.3.3 Application d'une fonction aux
#         élements d'une data.frame: la fonction tapply
tapply(Age,Sexe,mean)

tapply(Age, list(Sexe, Love),mean)

#2.10.3.4 Fonction qui construit une liste: la fonction split
split(Age,Sexe)

#3 Exercices de synthèse:

#3.1
conso.dat = read.table("donneesconsommation.txt", header=T})

attach(conso.dat)

#3.2 



conso.dat=read.table("donneesconso.txt",header=T)
attach(conso.dat)


compter=function(a,b) {
  d=numeric()
  for(i in 1:(length(a)))d[i]=sum(b==a[i])
  names(d)=as.character(a)
  d
}

nom.region=c('GR1','GR2','GR3')
nom.age=c('TA1','TA2')

conso.liste=list()

for(i in 1:3) {
  conso.liste[[i]]=list()
  for(j in 1:2) {
    conso.liste[[i]][[j]]=list()
    conso.liste[[i]][[j]][[1]]=Revenu[(Region==nom.region[i])
                                     &(Age==nom.age[j])]
    conso.liste[[i]][[j]][[2]]=Loisirs[(Region==nom.region[i])
                                      &(Age==nom.age[j])]
    conso.liste[[i]][[j]][[3]]=Pates[(Region==nom.region[i])
                                    &(Age==nom.age[j])]
    conso.liste[[i]][[j]][[4]]=
      table(Cafe[(Region==nom.region[i])&(Age==nom.age[j])])
    maxi=max(Enfants)
    conso.liste[[i]][[j]][[5]]=
      compter(0:maxi,Enfants[(Region==nom.region[i])
                             &(Age==nom.age[j])])
    conso.liste[[i]][[j]][[6]]=
      mean(Magnetoscope[(Region==nom.region[i])
                        &(Age==nom.age[j])])
    conso.liste[[i]][[j]][[7]]=
      mean(Lavevaisselle[(Region==nom.region[i])
                         &(Age==nom.age[j])])
    names(conso.liste[[i]][[j]])=names(conso.dat)[3:9]
  }
  names(conso.liste[[i]])=nom.age
}
names(conso.liste)=nom.region

