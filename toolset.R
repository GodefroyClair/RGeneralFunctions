require('tidyr')
require('signal')
require('dplyr')
require('fields')
require('shiny')
require('ggplot2')
require('gtable')
require('ggvis')

#######################
#####LOAD FUNCTIONS####
#######################

#helper function to check if the r script is running in RStudio or vim
rstudio.ide <- function(){
  if(Sys.getenv("RSTUDIO")!=1){
    jpeg(filename = "./graphs/occi-data%03d.jpg")
    return(T)
  }else return(F)
}

#path creation
get.data.path <- function(dir.name){
  #dir on different computers
  mac.gd.path <- "/Users/godot/githubRepos"
  hp.gd.path <- "C:/Users/Godefroy/githubRepos"
  
  if (grepl("bolivar", Sys.info()['THIM'])){
    gd.path <- hp.gd.path
  }else if (grepl("bolivar", try(system("echo $HOSTNAME", intern = T)))){
    gd.path <- mac.gd.path 
  }else stop("path error : unknown computer")
  
  data.path <- paste(gd.path,dir.name,"data/",sep = "/")
  
  if(!file.exists(data.path))stop("error : directory not found")
  data.path
}

##create a table data frame (see tbl_df in {dplyr})
csv2tbl.df <- function(filename, data.sep=";", head=T, dec.sep=","){
  
  if(!file.exists(filename))stop("error : directory not found")
  #chargement d'un fichier csv dans un data frame
  df <-read.csv2(file = filename, sep=data.sep,header = head,dec= dec.sep,na=NA)
   
  return(tbl_df(df))
}

###########################
#####END LOAD FUNCTIONS####
###########################


##############################
########PLOT FUNCTIONS########
##############################

##################################
########END PLOT FUNCTIONS########
##################################

#################################
#######TOOLSET FUNCTIONS########
##################################

##################################
########### FILTER ###############
##################################



##################################
############END ##################
##################################

##################################
##################################