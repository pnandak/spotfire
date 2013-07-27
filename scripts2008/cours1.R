##http://cran.cict.fr/bin/windows/base/R-2.6.1-win32.exe

## CREER DES OBJETS BASIQUES

n = 10
x = 1
x = 10
y = 10 + 2
z = 3+rnorm(1)
w = 8; name = "Arthur"; dicton = "Vive le logiciel R";

## LISTER LES OBJETS EN MEMOIRE

ls()
ls(pat = "n")
ls(pat = "^n")
ls.str()

## EFFACER LES OBJETS DE LA MEMOIRE

rm(x)
rm(n, z)
rm(list = ls())
rm(list = ls(pat = "^n"))

## L'AIDE EN LIGNE

help.search("regression")
?lm
help(lm)
apropos(lm)

help("*")


## LES DONNEES AVEC R

n = 8 ; n ; mode(n)
x = c(FALSE, T); x; mode(x); length(x)

A = "bonjour, bienvenue au premier cours de R"
A
mode(A)
length(A)

A = c("bonjour, bienvenue au premier cours de R", "vous allez découvrir un univers merveilleux")
A
mode(A)
length(A)


## LES VECTEURS

a = vector("numeric", 5)
b = vector("character", 5)
c = vector("logical", 5)
d = vector("complex", 5)
a;b;c;d

a = numeric(5)
b = character(5)
c = logical(5)
d = complex(5)
a;b;c;d

## LES MATRICES

matrix(0, 5, 7)
x = 1:20
mat1 = matrix(x, 4, 5)
mat2 = matrix(x, 4, 5, byrow = TRUE)
mat3 = matrix(x, 4, 5)
nom_var = paste("V", 1:5, sep = "")
nom_ind = paste("I", 1:4, sep = "")
colnames(mat3) = nom_var; rownames(mat3) = nom_ind;
dimnames(mat3) = list(nom_ind, nom_var)

## LES DATA.FRAME

## CREATION D'UNE DATA.FRAME

a = c(1, 2, 3)
b = c("a", "b", "c")
mode(a);mode(b)
df = data.frame(a, b)
df2 = data.frame(a = 1:6, b = matrix(1:24, 6, 4));df2

## ILLUSTRATION DU RECYCLAGE (QU'UN NOMBRE ENTIER DE FOIS)

a = c(1, 2, 3, 4, 5, 6)
b = c("a", "b", "c")
df = data.frame(a, b)

a = c(1, 2, 3, 4, 5)
df = data.frame(a, b)


## LES LISTES
a = c(1, 2, 3, 4, 5)
b = c("a", "b", "c")
liste1 = list(a, b)
liste1
names(liste1) = c("L1", "L2")

liste2 = list(L1 = a, L2 = b)
liste2

## CONVERSION D'OBJETS

## CONVERSION DE MODE

## 1.	VERS LE NUMERIQUE : as.numeric

## Logique vers numŽrique
logique = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE) 
conversion_numerique =  as.numeric(logique)

## caractere vers numŽrique
caractere1 = c("1", "2", "3", "4", "5") 
conversion_numerique1 =  as.numeric(caractere1)
caractere2 = c("A", "/", "T", "%", "-")
conversion_numerique2 =  as.numeric(caractere2)

## 2.	VERS LE LOGIQUE: as.logical

## Numerique vers logique
numerique = 0:10
conversion_logique1 = as.logical(numerique)

## Caractere vers le logique

caractere = c("FALSE", "TRUE", "F", "T", "false", "t", "A", "(")
conversion_logique2 = as.logical(caractere)


## 3.	VERS LE CARACTéRE: as.character

## numérique vers caractère
numerique = 1:8
conversion_caractere1 = as.character(numerique)

## logique vers caractère
logique = c(TRUE, FALSE)
conversion_caractere2 = as.character(logique)


## CONVERSION DE TYPE D'OBJETS

## conversion d'une matrice en une data.frame

a = matrix(1:25, nrow = 5, ncol = 5);a;
b = as.data.frame(a);b;

## Conversion d'un facteur en numŽrique

facteur = factor(c(1, 5, 10));facteur
facteur_numerique1 = as.numeric(facteur);facteur_numerique1;
facteur_caractere = as.character(facteur);facteur_caractere
facteur_numerique = as.numeric(facteur_caractere);facteur_numerique

## ACCEDER AUX VALEURS D'UN OBJET

## Accéder aux valeurs d'un objet par l'indexation

## Pour les matrices ou les vecteurs
x = 1:20;x;
x[x > 10] = 20;x;
x[x==20] = 0;x

a = matrix(1:25, 5, 5);a
a[3, 3] = 2;a
a[ , 3]
a[ , 3] = 3; a
a[3, ] = 3; a
a[, 3] = 11:15; a

x = c(1.1, 5.3, 9, 4.2, 3.6, 7.2, 8.4, 1.6, 8.8, 3.5);x;
x < 5 
y = x[x < 5];y
y = x[c(2, 6)];y

## selectionner les chiffres paires

x = 1:20
xpair1 = x[x%%2 == 0];xpair1
temp = c(F, T)
xpair2 = x[temp];xpair2

## Pour les data.frame

a = c(1, 2, 3)
b = c("a", "b", "c")
df = data.frame(a, b)
df[2][3, 1]
df[2][, 1] = 1
df2 = data.frame(a = 1:6, b = matrix(1:24, 6, 4));df2
df2[2]
df2[2:5]
df2[2:5][3, ]
df2[2][3, 1] = 999;df2
df2[2:5][3, ] = 999;df2

## Pour les list

liste = list(a = 1:6, b = matrix(1:24, 6, 4));liste
liste[[1]]
liste[[2]]
liste[[1]][c(TRUE, FALSE)]
liste[[2]][ ,3]

## ACCƒDER AUX VALEURS D'UN OBJET PAR LE NOM

liste = list(a = 1:6, b = matrix(1:24, 6, 4));liste
liste$a;liste$b
liste$b[1, ]

## ACCEDER AUX VALEURS D'UN OBJET GRåCE Ë L'ƒDITEUR


mat = matrix(rnorm(24), 6, 4);
data.entry(mat)
mat

X = list(a = 1:6, b = matrix(1:24, 6, 4));liste
data.entry(X$b)
X$b

## ------------------------------------------------------

x = 1:7
sum(x);log(x);sin(x);min(x);max(x);sqrt(x); mean(x); median(x)

## ------------------------------------------------------
vecteur = c(5, 7.2, 3.6, 4.9)
vecteur

y = c(1, -1, 1, -1)
y

produit = y*vecteur
produit

somme = y+vecteur
somme
	

y = c(1, -1)
y

produit = y*vecteur
produit

y = c(1, -1, 1)
y

y*vecteur
## ------------------------------------------------------ 
