
   
   # Ouverture d'une session R

   help.start() # S'il te plaît, R, aide-moi à démarrer !
   
   x = 2        # affectation de la valeur 2 dans l'objet appelé x  
   y = sqrt(x)  # affectation dans y du résultat de la fonction sqrt appliquée à x 
   y            # lecture de l'objet y 
   help(sqrt)   # S'il te plaît, R, dis moi tout sur la fonction sqrt
   
   x = 1:20     # affectation dans x des valeurs entières entre 1 et 20   
   y = sqrt(x)  # affectation dans y du résultat de la fonction sqrt appliquée à x 
   y            # lecture de y
   
   objects()    # dis moi, R, quels sont les noms des objets de la session ?
   objects(pattern="x") # dis moi, R, quels sont les noms des objets de la session contenant la lettre x ?
   
   # Environnement de programmation

   setwd("C:/chezmoi/")         # définit le répertoire C:/chezmoi comme répertoire de travail   
   source("ilfaitsoleil.R")     # lance les commandes du fichier ilfaitsoleil.R
   
   # Chargement de librairies externes

   source("http://www.bioconductor.org/biocLite.R") # Chargement du fichier de commandes biocLite.R  
                                                    # disponible sur la page web http://www.bioconductor.org
   biocLite() # La fonction biocLite est définie dans biocLite.R. Elle commande le   
              # téléchargement automatique des packages (version allégée de Bioconductor).
   
   
