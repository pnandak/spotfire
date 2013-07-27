## Fonction du cours 2
b = 3+rnorm(1)
a = rep(5, 100)
c = matrix(1:20, 4, 5)
d = list(a, b, c)

save(a, file = "D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/sauvegarde.Rdata")
save(a, b, file = "D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/sauvegarde1.Rdata")
save(list = ls(), file = "D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/sauvegarde2.Rdata")
getwd("")
setwd("")
save.image()

load("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/sauvegarde2.Rdata")
 

# IMPORTATION DE DONNEES

#Premier exemple : Auto original
A1 = read.table("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/auto2004_original.txt", header = TRUE, sep = "\t")
A2 = read.table("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/auto2004_sans_nom.txt", header = FALSE, sep = "\t")
A3 = read.table("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/auto2004_virgule.txt", header = TRUE, sep = "\t", dec = ",")
A4 = read.table("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/auto2004_don_manquante.txt", header = TRUE, sep = "\t", na.strings = " ")
A5 = read.table("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/auto2004_don_manquante(99999).txt", header = TRUE, sep = "\t", na.strings = "99999")


write.table(A1, "D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/temp1.txt")
B = read.csv("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/dead.csv", header = TRUE, sep = ",")

#Lecture de fichier Excel
library(xlsReadWrite)
C = read.xls("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/bordeaux.xls", colNames = TRUE, sheet = 1)
write.xls(C, "D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/temp2.xls")
#Lecture de Fichier SPSS ou SAS (library(foreign))
#Pour SPSS : 
? read.spss()
#Pour SAS : via la fonciton 
? read.ssd() 
#et 
? read.xport

#Pour sauver les données au format R
save(A1, B, C, file = ""D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/sauvegarde.Rdata")

save(list = ls(), file = ""D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/sauvegarde1.Rdata")

# GRAPHIQUES

demo(graphics)

## GESTION DES FENETRES GRAPHIQUES

x11() ; x11() ; x11()
dev.set(2) ; dev.set(3)

## UTILISATION DE LA FONCTION SPLIT.SCREEN ET LAYOUT

split.screen(c(1, 2));
screen(1);screen(2)
layout(matrix(1:4, 2, 2));layout.show(4)
layout(matrix(1:6, 2, 3));layout.show(6)
mat = matrix(1:6, 3, 2)
layout(mat);layout.show(6)

## CAS SIMPLE SUR LES AUTOMOBILES : Graphe de Puissance vs cylyndrée 
A = read.table("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/auto2004_original.txt", header = TRUE, sep = "\t")

?plot
plot(A$Puissance, A$Cylindree)
x11();plot(A[, 3], A[, 2])
plot(A[, 3], A[, 2], xlab = "Puissance", ylab = "Cylindree")
plot(A[, 3], A[, 2], xlab = "Puissance", ylab = "Cylindree", main = "Les automobiles")
plot(A[, 3], A[, 2], xlab = "Puissance", ylab = "Cylindree", main = "Les automobiles", col = "red")
plot(A[, 3], A[, 2], xlab = "Puissance", ylab = "Cylindree", main = "Les automobiles", col = "blue", pch = 22, bg = "red")
plot(A[, 3], A[, 2], xlab = "Puissance", ylab = "Cylindree", main = "Les automobiles", col = "blue", pch = 22, bg = "red", bty = "l")
plot(A[, 3], A[, 2], xlab = "Puissance", ylab = "Cylindree", main = "Les automobiles", col = "blue", pch = 22, bg = "red", bty = "l", tcl = -0.25)

plot( A[, 3], A[, 2], 
      xlab = "Puissance", 
      ylab = "Cylindree", 
      main = "Les automobiles", 
      col = "blue", 
      pch = 22, 
      bg = "red", 
      bty = "l", 
      tcl = -0.25,
      cex.lab = 1.5,
      cex.axis = 1.5,
      cex.main = 1.5)
      
# couleur des labels, couleurs des axes, couleur des titre .....
?par

# Un nouveau point arrive et on souhaite le ploter sur le graphe déjà existant.
Puis = 400
Cyl = 3500

points(Puis, Cyl, col = "yellow", pch = 23, bg = "red")

## CAS SIMPLE
C1 = matrix(rnorm(1000, sd = 0.5), 500, 2) 
C2 = matrix(rnorm(1000, mean = 1, sd = 0.5), 500, 2) 
plot(C1)
plot(C2)
hist(C1)
hist(C2)
hist(C1, 50)
hist(C2, 50)

plot(C1)
points(C2)

plot(C1, col = "blue")
points(C2, col = "red")

mat = rbind(C1, C2)
plot(C1, col = "blue", xlim = range(mat[ ,1]), ylim = range(mat[ ,2]))
points(C2, col = "red")

plot(C1, col = "blue", xlim = range(mat[ ,1]), ylim = range(mat[ ,2]), main = "représentation d'un nuage de points", xlab = "X1", ylab = "X2")
points(C2, col = "red")


## ILLUSTRATION DES OPTIONS GRAPHIQUES

plot(C1, type = "n", xlim = range(mat[ ,1]), ylim = range(mat[ ,2]), main = "représentation d'un nuage de points", xlab = "X1", ylab = "X2", , bty = "l", tcl = -.25)
rect(-3, -3, 3, 3, col = "cornsilk") 
points(C1, col = "blue", pch = 22, bg = "red")
points(C2, col = "red", pch = 25, bg = "yellow")


## On avance .......
plot(A)

plot(iris[ ,1:4], bg = c("red", "green3", "blue")[iris[ ,5]], pch = c(21, 25, 24)[iris[ ,5]], main = "Iris de Fisher", labels = c("Longueur\nSepale", "Largeur\nSepale","Longueur\nPetale", "Largeur\nPetale"))

## Envoyer le graphique dans un fichier pdf par exemple
setwd("D:/Documents and Settings/at215484/Bureau")
pdf("IRIS.pdf")
plot(iris[ ,1:4], bg = c("red", "green3", "blue")[iris[ ,5]], pch = c(21, 25, 24)[iris[ ,5]], main = "Iris de Fisher", labels = c("Longueur\nSepale", "Largeur\nSepale","Longueur\nPetale", "Largeur\nPetale"))
dev.off()
png("IRIS.png")
plot(iris[ ,1:4], bg = c("red", "green3", "blue")[iris[ ,5]], pch = c(21, 25, 24)[iris[ ,5]], main = "Iris de Fisher", labels = c("Longueur\nSepale", "Largeur\nSepale","Longueur\nPetale", "Largeur\nPetale"))
dev.off()
postscript("IRIS.ps")
plot(iris[ ,1:4], bg = c("red", "green3", "blue")[iris[ ,5]], pch = c(21, 25, 24)[iris[ ,5]], main = "Iris de Fisher", labels = c("Longueur\nSepale", "Largeur\nSepale","Longueur\nPetale", "Largeur\nPetale"))
dev.off()



## Exemple illustratif sympathique

n = 500
x = rpois(n, lambda = 2);y = rpois(n , lambda = 2)
layout(t(matrix(1:2)))
plot(x, y, pch = 19, main = "un nuage de point trompeur")
sunflowerplot(x, y, pch = 19, main = "un nuage de points moins trompeur")



#Les Histogrammes

library(MASS)
data(survey)
mat = matrix(c(1:2), 1, 2)
layout(mat)
layout.show(2)

hist(survey$Height, col = "yellow", border = "red", 
     main = paste("Taille de", nrow(survey), " étudiants"), 
     xlab = "Taille [cm]", ylab = "Effectifs", ylim = c(0, 50), 
     labels = TRUE)

hist(survey$Height, breaks = seq(from = 150, to = 200, length = 20),
     col = "green3", border = "sienna", 
     main = paste("Taille de", nrow(survey), " étudiants"), 
     xlab = "Taille [cm]", ylab = "densité", 
     proba = TRUE, labels = TRUE, ylim = c(0, 0.06))

x = seq(from = 150, to = 200, length = 100)
lines(x, dnorm(x, mean(survey$Height, na.rm = TRUE),
        sd(survey$Height, na.rm = TRUE)
    
 )
     )
mtext("Ajustement à une loi normale")

adj = 0.5
dst = density(survey$Height, na.rm = TRUE, adjust = adj)
hist(survey$Height, 
  breaks = seq(from = 150, to = 200, length = 20),
  col = "yellow", border = "red", 
  main = paste("Taille de", nrow(survey), " étudiants"), 
  xlab = "Taille [cm]", ylab = "densité", 
  proba = TRUE, labels = TRUE, ylim = c(0, 0.06))
lines(dst$x, dst$y, lwd = 2)
mtext(paste("adjust = ", adj))

adj = 1
dst = density(survey$Height, na.rm = TRUE, adjust = adj)
hist(survey$Height, 
  breaks = seq(from = 150, to = 200, length = 20),
  col = "yellow", border = "red", 
  main = paste("Taille de", nrow(survey), " étudiants"), 
  xlab = "Taille [cm]", ylab = "densité", 
  proba = TRUE, labels = TRUE, ylim = c(0, 0.06))
lines(dst$x, dst$y, lwd = 2)
mtext(paste("adjust = ", adj))


ng = sum(survey$Sex == "Male", na.rm = TRUE)
nf = sum(survey$Sex == "Female", na.rm = TRUE)
n <- ng + nf
dst = density(survey$Height, na.rm = TRUE)
dstg = density(survey$Height[survey$Sex == 
              "Male"], na.rm = TRUE )
dstf = density(survey$Height[survey$Sex == 
              "Female"], na.rm = TRUE )
hist(survey$Height, col = "yellow", 
     border = "red", 
     main= paste("Taille de", nrow(survey),
                  "étudiants" ),
     xlab = "Taille [cm]", proba = TRUE, 
     ylim = c(0, max(dst$y))
    )
lines(dstg$x, ng/n * dstg$y, lwd = 3, 
      col = "darkblue" )
lines(dstf$x, nf/n * dstf$y, lwd = 3, lty = 3, 
      col = "darkred" )
lines(dst$x, dst$y)
legend(185, 0.04, legend = c("Filles", "Garcons"), 
       col = c("darkred", "darkblue"),
       lty = c(3, 1), lwd = 2, pt.cex = 2 )

## BOXPLOT

library(MASS)
data(survey)
boxplot(survey$Height, 
  col = "sienna", 
  main = paste("Taille de", nrow(survey),
               "etudiants"
              ), 
  ylab = "Taille",
  las = 1
       )

rug(survey$Height, side = 2)


boxplot(survey$Height~survey$Sex, col = c("lightpink", "lightblue"), ylab = "taille", main = paste("Taille de", nrow(survey), "étudiants"))


#Camembert

vente.tarte = c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(vente.tarte) = c("Cerise", "Framboise ", "Pomme", 
   "Abricot", "Pêche", "Fraise"
  )
pie(vente.tarte,
    col = c("tan", "green3", " plum", 
            " royalblue", "red", "peru"),
    border = NA,
    main = "Ventes de tartes"


# Le package Lattice

library(lattice)
library(ade4)
data(deug)
x = deug$tab$Algebra
y = deug$result

densityplot(~ x | y, xlab = " note d’algèbre ", col = "red")

bwplot(~ x | y, main = "note d'algèbre")

histogram(~ x | y, col = "peru", main = "note d'algèbre", xlab = "histogramme")

splom(~iris[1:3] | Species, data = iris,
      pscales = 0,
      varnames = c("Sepal\nLength",
                   "Sepal\nWidth",
                   "Petal\nLength"
                  )
     )