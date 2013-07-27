###########################################
# Cours 3 : exemple d'analyse statistique #
###########################################

##############################################
# LES APPARTEMENTS PARISIENS : LA REGRESSION #
##############################################
library(xlsReadWrite);
appart = read.xls("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/appart.xls", colNames = TRUE, sheet = 1)

appart.lm = lm(appart$prix~appart$surface)

plot(appart$surface, appart$prix,
     xlab = "surface", ylab = "prix", 
     col.axis = "peru", 
     col.lab = "royalblue", 
     col.main = "royalblue", 
     main = " étude de 28 appartements"
    )

abline(appart.lm, col = "green3")

predict(appart.lm)
plot(appart$prix, predict(appart.lm))

layout(matrix(1:4, 2, 2))
plot(appart.lm)

##############################################
# LES IRIS DE FISHER : ANALYSE DISCRIMINANTE #
##############################################

plot(iris[ ,1:4], 
     bg = c("red", "green3", "blue")[iris[ ,5]], 
     pch = c(21, 25, 24)[iris[ ,5]], 
     main = "Iris de Fisher", 
     labels = c("Longueur\nSepale", "Largeur\nSepale","Longueur\nPetale", "Largeur\nPetale")
     )


library(MASS)
iris.lda = lda(iris[, 1:4], iris[, 5])



result.lda = predict(iris.lda, iris[ ,1:4])

plot(iris.lda, col = as.numeric(iris[ ,5]))


############################
# L'ALGORITHME DES K-MEANS #
############################

## Création d’une matrice de données artificielles 
## contenant deux sous-populations
C1 = matrix(rnorm(100, sd = 0.3), ncol = 2)
C2 = matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
mat = rbind(C1, C2)

## Visualisation des données générées
plot(C1, col = "royalblue", pch = 16, 
     xlim = range(mat[ ,1]), 
     ylim = range(mat[ , 2]),
     xlab = "premier axe",
     ylab = "second axe",
     main = "Nuage de 2 populations de points points"
    )
points(C2, col = "green3", pch = 15)

result.kmeans = kmeans(mat, 2)

## Visualisation graphique des résultats

layout(t(matrix(1:2)))
plot(C1, col = "royalblue", pch = 16, 
     xlim = range(mat[ ,1]), 
     ylim = range(mat[ , 2]),
     xlab = "premier axe",
     ylab = "second axe",
     main = "Méthodes des kmeans")

points(C2, col = "green3", pch = 15)

points(result.kmeans$centers, col = 1 :2, pch = 7, lwd = 3)

segments(mat[result.kmeans$cluster == 1, ][ , 1], 
         mat[result.kmeans$cluster == 1, ][ , 2],
	  result.kmeans$centers[1, 1],
	  result.kmeans$centers[1, 2],
	  col = 1
 )

segments(mat[result.kmeans$cluster == 2, ][ , 1], 
         mat[result.kmeans$cluster == 2, ][ , 2],
	  result.kmeans$centers[2, 1],
	  result.kmeans$centers[2, 2],
	  col = 2
 )



########################################################
# L'ANALYSE EN COMPOSANTE PRINCIPALE : LES AUTOMOBILES #
########################################################

auto = read.table("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/auto2004_original.txt", header = TRUE, sep = "\t")

auto.pca = princomp(auto, cor = TRUE)

rownames(auto) = auto[, 1]
auto = auto[, 2:7]

auto.pca = princomp(auto, cor = TRUE)
names(auto.pca)
biplot(auto.pca)

####################################################################
# REGRESSION SUR COMPOSANTES PRINCIPALES: LES DONNES CHOC SEPTIQUE #
####################################################################



###################################################################
# Chargement des données pour la régression logistique et les SVM #
###################################################################

library(svmpath)
data(svmpath)
X = mixture.data$x
Y = mixture.data$y ;  Y[which(Y == -1)] = 0

x11();plot(X, 
     col = Y+3, 
     pch = Y + 4, 
     main = "Mixture data", 
     xlab = "X1", 
     ylab = "X2"
    )

############################
# La régression logistique #
############################

result.glm = glm(Y ~ X, family = binomial)
summary(result.glm)
Y_predit = predict(result.glm, as.data.frame(X))

###############################
# Les Support Vector Machines #
###############################

library(kernlab)
Y = mixture.data$y ;
svp <- ksvm(X,Y,kernel = "vanilladot", kpar=list(), type = "C-svc")
x11();plot(svp,data=X)
svp <- ksvm(X,Y,kernel = "rbfdot", kpar=list(sigma= 3), type = "C-svc")
x11();plot(svp,data=X)
Ypred.svm = predict(svp, X)
svp <- ksvm(X,Y,kernel = "rbfdot", kpar=list(sigma= 100), type = "C-svc")
x11();plot(svp,data=X)

