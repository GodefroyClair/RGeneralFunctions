############################################################
#####################OCCI DATA TEST#########################
############################################################

#03/03/2016

#load libraries
library(knitr)
library(plyr)
library(grid)
library(ggplot2)
library(gtable)
library(ggvis)
librajjjy(reshape2)
library(grDevices)
library(kohonen)
library(zoo)
library(tidyr)
library(signal)
library(dplyr)
library(fields)
#ggplot theme (disable ugly grey theme, put a white & grey grid instead)
theme_set(theme_bw(24))

#include my ggplot & dplyr based libraries
source("dplyr-functions.R")
source("ggplot-functions.R")

get.data.path <- function(dir.name){
  #dir on different computers
  mac.gd.path <- "/Users/godot/githubRepos"
  hp.gd.path <- "C:/Users/Godefroy/githubRepos"
  
  if (grepl("bolivar", Sys.info()['THIM'])){
    gd.path <- hp.gd.path
  }else if (grepl("bolivar", Sys.info()['nodename'])){
    gd.path <- mac.gd.path 
  }else stop("path error : unknown computer")
  
  data.path <- paste(gd.path,dir.name,"data/",sep = "/")
  
  if(!file.exists(data.path))stop("error : directory not found")
  data.path
}

data.dir.path <- get.data.path("occi")

file.path <- paste(data.dir.path,"data.csv",sep = "")

##create a table data frame (see tbl_df in {dplyr})
csv2tbl.df <- function(filename, data.sep=";", head=T, dec.sep=","){
  
  if(!file.exists(filename))stop("error : directory not found")
  #chargement d'un fichier csv dans un data frame
  df <-read.csv2(file = filename, sep=data.sep,header = head,dec= dec.sep,na=NA)
  return(tbl_df(df))
}

#data are in csv format, sep by tab, US decimal
occi.tbl <- csv2tbl.df(file.path ,data.sep = "\t", dec.sep = ".")

######################
###DATA CLEANING######
######################

##1 Checking data

#verify that occi.tbl is a table data frame
if(!"tbl_df" %in% class(occi.tbl))("path error : problem with the import, not the right class")
#check dimensions
dim(occi.tbl)
#check col names
names(occi.tbl)
#change name (more explicit)
names(occi.tbl) <- c("trajectory.id","position.x","position.y","date.time","error")

#Check data structure
str(occi.tbl)
# View structure of data
str(occi.tbl)
#View structure of data using dplyr's glimpse()
glimpse(occi.tbl)
# View summary of data
summary(occi.tbl)

# View first 15 rows
head(occi.tbl, n = 15)
# View last 15 rows
tail(occi.tbl, n = 15)

##2 Formating data
#date.time column, format to 1/10 of a ms
occi.tbl$date.time<-  strptime(occi.tbl$date.time,format="%Y-%m-%d %H:%M:%OS") 
str(occi.tbl$date.time)

#create a new colum showing the time elapsed since the begining of the experiment (ie trajectory)
#we use the {base} class difftime
add.elapsed.time <- function(df){
  df$date.time <- as.POSIXct(df$date.time) #to be sure...
  df %>% group_by(trajectory.id) %>% mutate(elapsed.time = round(date.time - min(date.time),digits = 3))
}

occi.tbl <- add.elapsed.time(occi.tbl)
#check new variable
glimpse(occi.tbl)

#missing data ?
if( any(is.na(occi.tbl)) )print("des données sont manquantes")

#Is elapsed.time not increasing by trajectory ?
answ <- occi.tbl %>% group_by(trajectory.id) %>% summarise(is.unsorted(elapsed.time))
#any unsorted ?
any(answ[[2]]==TRUE)
#any stritly unsorted (i.e. time superposition) ?
ans2 <- occi.tbl %>% group_by(trajectory.id) %>% summarise(is.unsorted(elapsed.time,strictly = T))
any(ans2[[2]]==TRUE)
#yes : find out if position is different for same time
time.duplic <- which(duplicated(occi.tbl$elapsed.time)) #data with exact elapsed time
length(time.duplic)
occi.tbl[sort(c(time.duplic-1, time.duplic, time.duplic+1)),c(2,3,6)]
                                            
#Any extreme data in position.x and position.y ?
hist(occi.tbl$position.x)
hist(occi.tbl$position.y)
ggplot(data = occi.tbl, mapping = aes(position.x)) + geom_histogram()
ggplot(data = occi.tbl, aes(x = position.y)) + geom_histogram()

#simplify trajectory.id
levels(occi.tbl$trajectory.id)
occi.tbl %>% group_by(trajectory.id) %>% mutate(simple.id)

#add var
occi.tbl2 <- occi.tbl %>% group_by(trajectory.id) %>% mutate(diff.time = elapsed.time  - lag(elapsed.time))
occi.tbl3 <- occi.tbl2 %>% group_by(trajectory.id) %>% mutate(diff.x = position.x - lag(position.x))
occi.tbl3 <- occi.tbl3 %>% group_by(trajectory.id) %>% mutate(diff.y = position.y - lag(position.y))
#euclidian dist (??), lag 1
occi.tbl4 <- occi.tbl3 %>% group_by(trajectory.id) %>% mutate(dist.eucl.lag1 = sqrt(diff.x^2+diff.y^2))


#####DATA AREE TIDY !#####

##DATA MINING

#duration of each trajectory ?
duration.by.traj <- occi.tbl4 %>% group_by(trajectory.id) %>% summarise(max(elapsed.time))
#summary in minutes
summary((as.numeric(duration.by.traj$`max(elapsed.time)`)/60))
hist((as.numeric(duration.by.traj$`max(elapsed.time)`)/60),xlab = "temps (min)",main = "")


levels <- levels(occi.tbl4$trajectory.id)

#trajectory for 1st id
occi.traj1 <- occi.tbl4[occi.tbl4$trajectory.id==levels[1],]
ggplot(data = occi.traj1,
       aes(x = position.x, y = position.y, col = as.numeric(elapsed.time),alpha = .1)) + 
  geom_path(aes(size=error))

#trajectory by facet
g <- ggplot(data = occi.tbl4,
       aes(x = position.x, y = position.y,col = as.numeric(elapsed.time),alpha = .1)) + 
  facet_wrap(~trajectory.id, ncol = 4) +
  scale_color_continuous(name="temps écoulé)", low = "blue",high = "red") + #color scale of color
  geom_path(aes(size = error)) +
  theme(legend.position = "none", #drop legend

        strip.text = element_text(size=rel(.3)),
        strip.background = element_rect( size = 0, fill = NA)
        ) 
grob <- ggplotGrob(g)
grob$heights[seq(3,length(grob$heights),by=4)] <- unit(.2,"lines")
grid.newpage()
grid.draw(grob)


#ideas
#space discretization ?
#time discretization ?
#given a time & a period, return a sub data frame of the data in the period around the time
time.frame.cut <- function(df, time, period = 10){
  df[df$date.time >= (time - period/2) & df$date.time <= time + period/2,]
}

dist.max <- function(df, distance = 5){
  mat <- matrix(c(df$position.x, df$position.y),nrow = length(df$position.x))
  max(rdist(mat))
}

