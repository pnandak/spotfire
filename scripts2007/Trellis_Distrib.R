## Comparaisons de distributions avec les graphiques Trellis
## Matthieu Lesnoff (CIRAD EMVT)


#--- Importation des données

library(lattice) # active la libairie lattice
trellis.device(color = 0) # spécifie un fond transparent

mydata <- read.table(
    file = "D:/Users/Metomet/GuR/Donnees/pat90sen.txt",
    header = TRUE,
    sep = ";"
    )
    
mydata

#--- Un facteur de variation

    #-- Histogrammes

histogram(formula = ~ poids | typnai, data = mydata,
    layout = c(1, 2))
    
histogram(
    formula = ~ poids | typnai,
    data = mydata,
    col = "orangered",
    n = 30,
    layout = c(1, 2),
    xlab = "Poids (kg)",
    ylab = "Pourcentage du total",
    main = "Agneaux à 90 jours"
    )

    #-- Graphiques des densités de probabilité

densityplot(formula = ~ poids | typnai, data = mydata,
    layout = c(1, 2))
    
densityplot(
    formula = ~ poids | typnai,
    data = mydata,
    col = "red",
    layout = c(1, 2),
    xlab = "Poids (kg)",
    ylab = "Densité de probabilité",
    main = "Agneaux à 90 jours"
    )

    #-- Box plots

bwplot(formula = poids ~ typnai, data = mydata)

bwplot(
    formula = poids ~ typnai, 
    data = mydata,,
    col = "red",
    fill = "wheat1",
    box.ratio = 0.2,
    scales = list(x = list(tck = 1)),
    xlab = "Type de naissance",
    ylab = "Poids (kg)",
    main = "Agneaux à 90 jours"
    )

    #-- Strip plots

stripplot(formula = poids ~ typnai, data = mydata,
    jitter.data = TRUE)

stripplot(
    formula = poids ~ typnai,
    data = mydata,
    groups = typnai, 
    col = c("red", "blue"),
    pch = 1,
    jitter.data = TRUE,
    factor = 0.9,
    scales = list(x = list(tck = 1)),
    xlab = "Type de naissance",
    ylab = "Poids (kg)",
    main = "Agneaux à 90 jours"
    )
    
stripplot(
    formula = poids ~ typnai,
    data = mydata,
    groups = typnai, 
    col = c("red", "blue"),
    pch = 1,
    jitter.data = TRUE,
    factor = 0.9,
    panel = function(x,y, ...) {
        panel.stripplot(x, y, ...)
        panel.points(
            x = 1,
            y = median(mydata$poids[mydata$typnai == "1PRO"]),
            pch = 16, cex = 1, col = "black"
            )
        panel.points(
            x = 2,
            y = median(mydata$poids[mydata$typnai == "2PRO"]),
            pch = 16, cex = 1, col = "black"
            )
        },        
    scales = list(x = list(tck = 1)),
    xlab = "Type de naissance",
    ylab = "Poids (kg)",
    main = "Agneaux à 90 jours"
    )
    
    #-- Graphiques des quantiles (Q-Q plots)

qq(formula = typnai ~ poids, data = mydata)

qq(
    formula = typnai ~ poids,
    data = mydata,
    panel = function(x,y, ...) {
        panel.qq(x, y, ...)
        panel.abline(0, 1, col = "red")
        },
    col = "blue",
    xlab = "Poids 1PRO (kg)",
    ylab = "Poids 2PRO (kg)",
    main = "Agneaux à 90 jours"
    )
    
#--- Deux facteurs de variation

    #-- Boxplots

bwplot(formula = poids ~ typnai:sexe , data = mydata)

bwplot(formula = poids ~ sexe | typnai , data = mydata)

bwplot(
    formula = poids ~ sexe | typnai, 
    data = mydata,
    col = "red",
    fill = "wheat1",
    box.ratio = 0.5,
    scales = list(x = list(tck = 1)),
    xlab = "Sexe",
    ylab = "Poids (kg)",
    main = "Agneaux à 90 jours"
    )



    
