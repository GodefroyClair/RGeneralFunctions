################################################################
###############fonctions de nettoyage et filtrage################
################################################################

###################
### 1 nettoyage ###
###################

#fonction qui retire les valeurs ('val') d'une des variables d'un dataframe
clean.data <- function(df, var.to.clean, val = 0){
  
  return(df[!(var.to.clean == val),])
}

clean.data(data.AW, "respiration")
data.AB[!("respiration" == 0),]
head(data.AB)
data.AW$date[1]
