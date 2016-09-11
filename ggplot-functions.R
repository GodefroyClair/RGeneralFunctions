##################################################################
########################GRAPHICAL FUNCTIONS######################
##################################################################

df<- structure(list(
                    X1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 
29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41), 
                    X2 = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 
                                                            280, 290, 300, 310, 320, 330, 340, 350, 360, 370, 380, 390, 400
)), .Names = c("X1", "X2"), row.names = c(NA, -41L), class = "data.frame")


plot.var <- function(df, mesure = "respiration", titre ="", names = "?" ){ 
     ggplot(data = df, aes(x = date, y= respiration)) +
         geom_line(size=.2, color="blue") +
         xlab("temps (m:s)") + 
         ylab(mesure) + 
         ggtitle(paste("Expérience",names,sep = " ")) +
         #geom_line(mapping = aes(y=lo,color="lo"),color="red",show.legend = T)+
         #geom_line(mapping = aes(y=lo2),color="green")+
         #geom_line(mapping = aes(y=lo3),color="brown")+
         #geom_line(mapping = aes(y=low),color="black")+
         #geom_line(mapping = aes(y=low2),color="yellow")+
         #theme(legend.title = element_text( size=4),axis.text.x = element_text(angle=90, vjust=1, size = 5))+
         #  scale_y_continuous(limits=c(-17,-3)) +
         scale_x_datetime(date_labels = '%M:%S' , date_breaks= "15 sec") +
         #  scale_x_datetime(date_labels = '%M:%S' , date_breaks= "30 sec", limit= c(debut, fin))
         #  geom_smooth(method="loess", formula = y ~ x,se=TRUE, size= 1, span=my.span) +
         #  geom_smooth(method="lm", se=TRUE, fill=NA,formula= y ~ poly(x,npoly),colour="red")+
         #  geom_smooth(method="loess", se=TRUE, fill=NA,formula= y ~ poly(x,2),colour="green",span=.1)
         geom_text(label="")
     #if(rstudio.ide()) dev.off()

 }


 #plot l'évolution par expérience
 plot.evol.par.expe <- function(df, mesure = "respiration",
                                titre="Evolution par expérience", lim.bas=NULL, lim.haut=NULL){
     #studio.ide()
     g <- ggplot(data = df, aes(x = as.numeric(tps.ecoule))) +
         facet_wrap(~nom.experience, ncol=2) +
         geom_line(mapping = aes_string(y=mesure),size = .2, color ="blue") +
         xlab("temps (s)") + 
         ggtitle(titre) +
         #scale_x_datetime(date_labels = '%M:%S' , date_breaks= "1 min") +
         theme(title = element_text(size=rel(.5)),
               axis.text = element_text(size = rel(.4)),
               axis.title=element_text(size=rel(.7)),
               strip.text = element_text(size=rel(.4)),
               strip.background = element_rect( size = 0, fill = NA),
               panel.margin = unit(0, "lines")
               )
         if(is.numeric(lim.bas) & is.numeric(lim.haut))g <- g + ylim(c(lim.bas,lim.haut))
         grob <- ggplotGrob(g)
         grob$heights[seq(3,length(grob$heights),by=4)] <- unit(.5,"lines")
         grid.newpage()
         grid.draw(grob)

         #if(rstudio.ide()) dev.off()
 }

chrono.plot.direct <- function(data, variable, transp = 1, taille = nrow(data), offset = 0){
  g <- ggplot(data[offset:(offset + taille),], aes_string("date",variable), alpha = transp) +
           geom_line(color = "blue") +
           xlab("temps (sec)") +
           ggtitle(data$nom.experience[1])
  g
}

chrono.plot.ellipse <- function(data, variable, transp = 1, taille = nrow(data), offset = 0){
  g <- ggplot(data, aes_string("date",variable), alpha = transp) +
      geom_line(color = "blue") +
      stat_ellipse(data[offset:(offset + taille),], mapping =aes_string("date", variable), color= "red") +
      #stat_ellipse(data = data[offset:(offset + taille),]) +
      xlab("temps (sec)") +
      ggtitle(data$nom.experience[1])
  g
}

##############################################
################OCCI FUNCTIONS#################
###############################################

plot.points <- function(df, alpha.factor = .1, color.factor = "as.numeric(elapsed.time)", 
                        leg.col.title = "temps écoulé", size.factor = "error", plot.title = ""){
    g <- ggplot(data = df, alpha = "alpha.factor",
                aes_string(x = "position.x", y = "position.y",   #mapping aesthetic
                           color = color.factor, size = size.factor)) + 
               geom_point(size=1/12) +
                   ggtitle(label = plot.title) +
                   guides(color = guide_legend(title = leg.col.title)) + #gen. case
                   theme(#legend.position = "none", #drop legend
                         text = element_text(size=rel(3)),
                         plot.title = element_text(size=rel(4)),
                         strip.background = element_rect( size = 0, fill = NA)
                         )

                   #continuous var for color
                   #2b improved...
                   if(grepl("as.numeric", color.factor)) {
                       g <- g + guides(color = guide_colourbar(title = leg.col.title, reverse = T))+
                           scale_color_gradient(low = "#56B1F7", high = "#132B43")
                   }
                   g
}


#function to plot trajectory from a df on a 2D map
#@default arg:
#   alpha.factor for transparency (default : .1)
#   color.factor for point color (def : time evolution)
#   size.factor for thickness of the point (def : error)
plot.trajectory <- function(df, alpha.factor = .1, color.factor = "as.numeric(elapsed.time)", 
                            leg.col.title = "temps écoulé", size.factor = "error", plot.title = ""){
    g <- ggplot(data = df, alpha = "alpha.factor",
                aes_string(x = "position.x", y = "position.y",   #mapping aesthetic
                           color = color.factor, size = size.factor)) + 
               geom_path() +
                   ggtitle(label = plot.title) +
                   guides(color = guide_legend(title = leg.col.title)) + #gen. case
                   theme(#legend.position = "none", #drop legend
                         text = element_text(size=rel(3)),
                         plot.title = element_text(size=rel(4)),
                         strip.background = element_rect( size = 0, fill = NA)
                         ) 

                   #continuous var for color
                   #2b improved...
                   if(grepl("as.numeric", color.factor)) {
                       g <- g + guides(color = guide_colourbar(title = leg.col.title, reverse = T))+
                           scale_color_gradient(low = "#56B1F7", high = "#132B43")
                   }
                   g
}

#trajectories split by trajectory id in facets
plot.trajectory.facets <- function(df, col.factor = "as.numeric(elapsed.time)", alpha.factor = ".1",  size.factor = "error", 
                                   leg.col.title = "temps écoulé", plot.title = ""){
    g <- ggplot(data = df, alpha = alpha.factor,
                aes_string(x = "position.x", y = "position.y", col = col.factor, size = size.factor)) +  #mapping aesthetic
               facet_wrap(~simple.id, ncol = 4) +
                   geom_path() +
                   ggtitle(label = plot.title) +
                   guides( color = guide_legend(title = leg.col.title)) + #color gen. case
                   theme(#legend.position = "none", #drop legend
                         text = element_text(size=rel(3)),
                         plot.title = element_text(size=rel(4)),
                         legend.text = element_text(size=rel(4)),
                         strip.background = element_rect( size = 0, fill = NA)
                         ) 
                   #if the color factor is on a continuous scale, override title...
                   if(grepl("as.numeric", col.factor)) {
                       g <- g + guides(color = guide_colourbar(title = leg.col.title, reverse = T))+
                           scale_color_gradient(low = "#56B1F7", high = "#132B43")
                   }
                   #drop strip rectangle above each facet, only keep title
                   grob <- ggplotGrob(g)
                   grob$heights[seq(3,length(grob$heights),by=4)] <- unit(.2,"lines") #dirty...
                   grid.newpage()
                   grid.draw(grob)
}

##plot distance by time
plot.dist.time <- function(df, color.factor = NULL, size = 1/5, plot.title = ""){
    g <- ggplot(data = df, aes(x = as.numeric(elapsed.time), y = dist)) +  #mapping aesthetic
        geom_point(aes_string(col=color.factor), size= size, alpha = .2) +
            ggtitle(label = plot.title) +
            xlab("temps écoulé (sec)")  +
            ylab("distance (mètre)")  +
            theme(#legend.position = "none", #drop legend
                  text = element_text(size=rel(3)),
                  plot.title = element_text(size=rel(4)),
                  legend.text = element_text(size=rel(4)),
                  strip.background = element_rect( size = 0, fill = NA)
                  ) 
            g
}

##plot distance by time with facets on trajectory id
plot.dist.time.facets <- function(df, plot.title = ""){
    g <- ggplot(data = df, aes(x = as.numeric(elapsed.time), y = dist)) +  #mapping aesthetic
        facet_wrap(~simple.id, ncol = 4) +
            geom_point(size=1/15, alpha = .05) +
            ggtitle(label = plot.title) +
            xlab("temps écoulé (sec)")  +
            ylab("distance (mètre)")  +
            theme(legend.position = "none", #drop legend
                  axis.text = element_text(size=rel(.1)),
                  strip.text = element_text(size=.5),
                  plot.title = element_text(size=1),
                  strip.background = element_rect( size = 0, fill = NA)
                  ) 

            #drop strip rectangle above each facet, only keep title
            grob <- ggplotGrob(g)
            grob$heights[seq(3,length(grob$heights),by=4)] <- unit(.2,"lines") #dirty...
            grid.newpage()
            grid.draw(grob)
}

#RAB
# grob <- ggplotGrob(g)
#grob$heights[[3]] = unit(.5,"lines")
#grob$heights[[7]] = unit(.5,"lines")
#print(grob$layout[grep("strip_t-.",grob$layout$name),])
#grob$layout[grep("strip_t-.",grob$layout$name),]$t <- grob$layout[grep("strip_t-.",grob$layout$name),]$t + 1
#grob$layout[grep("strip_t-.",grob$layout$name),]$b <- grob$layout[grep("strip_t-.",grob$layout$name),]$b + .5
#grid.newpage()
#grid.draw(grob)
#plot.margin =  unit(c(-0.1, -0.1, 0, 0), "lines")
#panel.margin = unit(-0.1, "lines")


####################SIGNAL#################
##DEBUG
#df_spctr <- df_spectre
#rm_zero <- T
#x_lim <- c(-.1,.1)

##visualisation du spectre de fréquence :
#made to work with the df returned by function spectrum analysis
#rm_zero allows one to remove the fft value at zero frequency
plot_spectrum <- function(df_spctr,x_lim = NULL, rm_zero = F, nom_exp = "") {
  if(rm_zero)df_spctr <- df_spctr[! df_spctr$freq_domain == 0, ]
  ggplot(df_spctr, aes(x = freq_domain, y = fft)) + 
    ggtitle(paste("expérience" , nom_exp)) +
    geom_line(col="red") +
    {if(!is.null(x_lim))xlim(x_lim)}
}
