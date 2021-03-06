
   
   # Ouverture d'une session R

   help.start() # S'il te pla�t, R, aide-moi � d�marrer !
   
   x = 2        # affectation de la valeur 2 dans l'objet appel� x  
   y = sqrt(x)  # affectation dans y du r�sultat de la fonction sqrt appliqu�e � x 
   y            # lecture de l'objet y 
   help(sqrt)   # S'il te pla�t, R, dis moi tout sur la fonction sqrt
   
   x = 1:20     # affectation dans x des valeurs enti�res entre 1 et 20   
   y = sqrt(x)  # affectation dans y du r�sultat de la fonction sqrt appliqu�e � x 
   y            # lecture de y
   
   objects()    # dis moi, R, quels sont les noms des objets de la session ?
   objects(pattern="x") # dis moi, R, quels sont les noms des objets de la session contenant la lettre x ?
   
   # Environnement de programmation

   setwd("C:/chezmoi/")         # d�finit le r�pertoire C:/chezmoi comme r�pertoire de travail   
   source("ilfaitsoleil.R")     # lance les commandes du fichier ilfaitsoleil.R
   
   # Chargement de librairies externes

   source("http://www.bioconductor.org/biocLite.R") # Chargement du fichier de commandes biocLite.R  
                                                    # disponible sur la page web http://www.bioconductor.org
   biocLite() # La fonction biocLite est d�finie dans biocLite.R. Elle commande le   
              # t�l�chargement automatique des packages (version all�g�e de Bioconductor).
   
   
