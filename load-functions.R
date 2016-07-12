#################################################
##############LOAD FUNCTIONS ####################
#################################################

load.file <- function(filename){
  
  #chargement d'un fichier csv dans un data frame, le CSV a une ligne pour les noms de variable (header = TRUE), les variables des colonnes sont respectivement de type chaîne de caractère et numérique (x3) au chargement
  df <-read.csv2(file = filename, sep=";",header = TRUE,dec=",",colClasses=c("character", rep("numeric",4)),na=NA)
  
  #modification des noms de variables
  names(df)<-c("date","respiration", "activite.electrodermale", "temperature", "frequence.cardiaque")
  
  #mise au format data/time de la 1ère variable de mesure de l'écoulement du temps
  df$date <- as.POSIXct(strptime(df$date,format="%d/%m/%Y %H:%M:%OS"))
  
  
  #ajout d'un variable catégorielles qui divise la durée de l'expérience en quatre quart temps #utile pour la vérification des données par la suite.
  df$quart.temps <- cut(1:nrow(df), breaks = 4, labels = c("1er","2eme","3eme","4eme"))
  
  #ajout d'une variable pour l'origine de l'expérience
  #en deux temps : 1) retirer ce qui est à droite du dernier / 2) on retire l'extension (.csv)
  filename <- gsub(".*/","",filename)
  df$nom.experience <- gsub("_.*", "",filename)
  
  return(tbl_df(df))
}
