######################################
# Cours 4 : exemple de programmation #
######################################

####################
# première exemple #
####################

ecriture_for = function(nombre)
{
	for (i in 1 : nombre)
	{
		print(i)
	}
}


####################
# deuxième exemple #
####################

ecriture_while = function(nbre)
{
i = 1
while (i <= nbre)
{
print(i)
i = i+1
}
}


##############################
# Troisième exemple avec for #
##############################

ecriture_pair = function(nombre)
{
for (i in 1: nombre)
	{
	 if (i%%2 == 0)
		{	
		 print(i)
		}
	}
}


##################################
# Troisième bis exemple avec for #
##################################

ecriture_pair = function(nombre)
{
for (i in 1: nombre)
	 {
	   if (i%%2 == 0 && i != nombre)
		    {	
		      print(i)
		    }
    }
}



##################################
# Troisième ter exemple avec for #
##################################

ecriture_pair = function(nombre)
{
for (i in 1: nombre)
	{
	 if (i%%2 == 0 || i != nombre)
		{	
		 print(i)
		}
	}
}


#######################################
# quatrième exemple avec while (bête) #
#######################################

ecriture_pair = function(nombre)
{
i = 1
while (i <= nombre)
	{
	 if (i%%2 == 0)
		{	
		 print(i)
		}
	 i = i+1
	}
}


##############################################
# quatrième exemple avec while (intelligent) #
##############################################

ecriture_pair = function(nombre)
{
i = 2
while (i <= nombre)
	{
	 print(i)
	 i = i+2
	}
}


#####################
# Cinquième exemple #
#####################

pair_carre = function(nombre)
{
if (nombre%%2 == 0)
print(paste("Le nombre", nombre, "est pair"))
else
print(paste("Le nombre ", nombre, " est impair"))
carre = nombre^2
print(paste("qu'il soit pair ou impair, une chose est sure, le carre de", nombre, "est ", carre))
}


###################
# Sixième exemple #
###################

factoriel1 = function(nombre)
{
facto = 1
for (i in 1 : nombre)
facto = facto*i

print(paste(" Le factoriel de", nombre, "est", facto))
}

sekou = function(V, seuil)
{
for (i in 1:length(V)){
  if (V[i]<seuil)
  { 
    print(V[i])
  }
}
}
####################
# Septième exemple #
####################

factoriel2 = function(nombre)
{
facto = 1
i = 1
while (i <= nombre)
{
facto = facto*i
i = i+1
}
print(paste(" Le factoriel de", nombre, "est", facto))
}

#######################
# sauvegarde en série #
#######################

for (i in 1: 10){
write(matrix(rnorm(100), 10, 10), file = paste("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/sauvegarde/A", i, ".txt"), ncolumns = 10)
}

##########################################################
# huitième exemple : exponentiel et développement limité #
##########################################################

#exp(x) = 1 + x + x^2/2! + x^3/3! + x^4/4! +...
exponentiel = function(x, n)
{
  sum(x^(0:n)/factorial(0:n))
}

#sin(x)= x - x^3/3! + x^5/5! - x^7/7! + ...
sinus = function(x, n){sum(rep(c(1, -1), length(seq(1, n, by = 2)))[1:length(seq(1, n, by = 2))]*(x^(seq(1, n, by = 2))/factorial(seq(1, n, by = 2)))) }



plot(seq(-10, 10, length = 200), sin(seq(-10, 10, length = 200)), ylim = c(-5, 5))
for (i in 1:length(seq(-10, 10, length = 200))){
points(seq(-10, 10, length = 200)[i], sinus(seq(-10, 10, length = 200)[i], 3), col = "red")
points(seq(-10, 10, length = 200)[i], sinus(seq(-10, 10, length = 200)[i], 5), col = "blue")
points(seq(-10, 10, length = 200)[i], sinus(seq(-10, 10, length = 200)[i], 10), col = "green")
points(seq(-10, 10, length = 200)[i], sinus(seq(-10, 10, length = 200)[i], 20), col = "yellow")
points(seq(-10, 10, length = 200)[i], sinus(seq(-10, 10, length = 200)[i], 50), col = "pink")
points(seq(-10, 10, length = 200)[i], sinus(seq(-10, 10, length = 200)[i], 100), col = "grey")
}
####################
# neuvième exemple #
####################

identite = function(dimension)
{
	X = matrix(0, dimension, dimension)
	for (i in 1:dimension)
	{
		for (j in 1: dimension)
		{
			if (i==j)
			X[i, j] = 1
		}
	}
	return(X)
}

############################################################################
# dixième exemple : stockage des coefficients de la régression logistique #
############################################################################

coef_logistic = function(X, Y)
{
b = numeric(dim(X)[2])
 for (i in 1 : ncol(X))
	{
	result.polr = polr(as.factor(Y) ~ X[ , i])
	b[i] = result.polr[[1]][1]
	}
return(b)

}

###################################################
# Application de la fonction aux vins de bordeaux #
###################################################
library(xlsReadWrite)
A = read.xls("D:/Documents and Settings/at215484/Bureau/Arthur/Bibliotheque/Cours R/donnees R (importation)/bordeaux.xls", colNames = TRUE, sheet = 1)
X = A[ , 2:5]
Y = A[ ,6]
library(MASS)
b = coef_logistic(X, Y)



