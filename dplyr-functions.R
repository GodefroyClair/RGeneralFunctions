#######################################
#################DPLYR#################
#######################################

#fonction de calcul du tps écoulé depuis le début de l'expe
#permet la comparaison entre expé (au contraire de la var "date" qui ne part pas d'un "tps zéro")
#template :
#df.all <- tbl_df(df.all)
#df.all$date <- as.POSIXct(df.all$date)
#df.all <- df.all %>% group_by(nom.experience) %>% mutate(tps.ecoule = date - min(date))
#RMQ : doit être au format tbl_df
fun.calc.duree <- function(df.affec){
  #just in case...
  df.affec$date <- as.POSIXct(df.affec$date)
  
  #ajoute une varaible tps.ecoule
  df.affec <- df.affec %>% group_by(nom.experience) %>% mutate(tps.ecoule = date - min(date))
  df.affec
}

