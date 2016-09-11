###############################################################
#################### CLEANING FUNCTIONS #######################
###############################################################

#libraries necessary :
library(stats)
library(signal)
library(purrr)

################ Cleaning signal ################
#usefull link :
#http://stackoverflow.com/questions/7105962/how-do-i-run-a-high-pass-or-low-pass-filter-on-data-points-in-r

##Low pass filter
##used with respiration data in affective computing project
#the idea is to clean all the "noise"
clean_high_freq <- function(df_col, threshold){
 low_pass_filter <- butter(2, threshold, "low") 
 signal::filtfilt(low_pass_filter,df_col) #filtfilt is used instead of filter to get rid of signal shift
}

#get the real spectrum analysis of a signal
fft_real <- function(signal){
  Mod(fft(signal)) 
}

##DEBUG
#signal <- data_PCo$respiration
#date <- data_PCo$date

#give the spectrum analysis of a signal sampled at the time given by date
#source : https://stat.ethz.ch/pipermail/r-help/2005-August/078197.html
#return a dataframe with two vectors of the size and the type of signal
spectrum_analysis <- function(signal, date){
  ## Find the sample period:
  if((delta <- as.numeric(date[2] - date[1])) == 0) stop("no time difference...")
  
  signal <- signal - mean(signal)
  
  ## Calculate the Nyquist frequency:
  f.Nyquist <- 1 / 2 / delta
  ## Calculate the frequencies.  (Since date smalest scale is in seconds, delta
  ## is in seconds, f.Nyquist is in Hz and freq is in Hz)
  ## (Note: I may be off by 1 in indexing here ????)
  freq_domain <- f.Nyquist*c(seq(length(signal)/2), 0, - rev(seq((length(signal)-1)/2)))/(length(signal)/2)
  fft <- fft_real(signal)
  data.frame(freq_domain, fft)
}


#DEBUG:
#df <-spectrum.analysis(data.DE[1:nrow(data.DE)-1,]$respiration,data.DE$date)

#regr.assymp <- lm(data.AB$respiration ~ stats::poly(data.AB$date,2))
#pred <- predict(regr.assymp)
#data.DE$respi.clean.learn <- cleaning.learning.effect(data.DE$respiration,data.DE$date)



#ggplot(data.DE, aes(x= date, y= respiration))  +
#  geom_line(color="green") + geom_line(aes(y=respi.clean.learn), color="red")


###################################################################
######################### CLEANING TRENDS #########################
########################### REGRESSIONS ############################
###################################################################

#clean the trend
clean_low_freq <- function(df_col, threshold = .01){
 high_pass_filter <- butter(2, threshold, "high") 
 filtered <- signal::filtfilt(high_pass_filter,df_col) #filtfilt is used instead of filter to get rid of signal shift
 #filtered[1:500] <- mean(filtered[501:1000])
 # nr <- length(df_col)
 # filtered[(nr-100):nr] <- mean(filtered[(nr-200):(nr-101)])
  filtered
}



plot_clean_low_freq <- function(df, threshold = .01) {
  df$respi_flat <- clean_low_freq(df$respiration, threshold)
  gg <- ggplot(df, aes(date, respiration)) + geom_line(col="red") +
    geom_line(aes(y = respi_flat), col ="blue" ) +
    ggtitle(df$nom.experience[1])
  print(gg)
  Sys.sleep(2)
}
#QUICK TEST
if(DEBUG & PRINT) {
  get_expe <- get_expe_base(df_all) #don't forget 
  df <- get_expe("CLP")
  plot_clean_low_freq(df)
}


plot_low_freq <- function(df, threshold = .01) {
  df$respi_flat <- clean_high_freq(df$respiration, threshold)
  gg <- ggplot(df, aes(date, respi_flat)) + geom_line(col="blue") +
    ggtitle(df$nom.experience[1])
  print(gg)
}

if(DEBUG & PRINT) {
  get_expe <- get_expe_base(df_all)
  df <- get_expe("CLP")
  plot_low_freq(df)
  walk(list_exp, ~ plot_low_freq(get_expe(.)))
  walk(list_exp, ~ plot_low_freq(get_expe(.)))
}



#finds an exponential trend
find_exp_trend <- function(df, debug = F) {
  ifelse(debug == T, { trc <- T ; smry <- T}, 
         {trc <- F; smry <- F })
  #in milli seconds
  df$msec_ecoule <- round(unclass(df$date) - unclass(df$date[1]), digits = 2) * 100
  
  a_start <- median(df$respi_clean[1:10]) #param a is the y value when x=0
  b_start <- 2*log(2)/(a_start*1000) #b is the decay rate
  b_start <- 0.0001 #b is the decay rate
  c_start <- mean(df$respi_clean) #general distance from zero ?? use median ?
  
  #from this graph set approximate starting values
  m <- nls(respi_clean ~ I(a * exp(-b * msec_ecoule) + c), data=df, 
           start=list(a = a_start, b = b_start, c = c_start), trace= trc)
  df$respi_trend <- predict(m,df$msec_ecoule)
  if(smry)summary(m)
  df
}
#DEBUG
if(DEBUG & PRINT) {
  get_expe <- get_expe_base(df_all)
  df <- get_expe("AB")
  df <- find_exp_trend(df, F)
  plot(df$msec_ecoule / 100, df$respi_clean, type = "l", col = "red")
  lines(df$msec_ecoule/100, df$respi_trend)
  plot(df$msec_ecoule/100,df$respi_clean-respi_est,type = "l", col = "green")
}

###########################################################################
########################## STAT PER PERIOD ################################
###########################################################################

# HIGH ORDER function that substitutes a signal divided in sequence by a boolean vector by a simple stat (max, min, mean...)
# The stat can be more complex...
# The function requires:
# signal : a signal (dah!!)
# periods : a vector of increasing (?) numeric values.
#           period "id" to which the corresponding element of the signal belongs
# stat : a stat function
# TODO : add the possibility to add subsequencies (for min max ??)
stat_per_period <- function(signal, periods) {
  
  return(function(stat) {
    #check if belong2seq is increasing
    if(is.unsorted(periods)) stop("ERROR : belong2seq not increasing seq...")
    if(!is.function(stat)) stop("ERROR : stat is not a function..")
   
    #divide signal into its periods 
    split_by_period <- split(signal, periods)
    #substitute each sequence by a stat calculated on this sequence
    sub_period_by_stat <- split_by_period %>% 
      purrr::map(function(lst) rep(stat(lst),length(lst)))
    
    unname(unlist(sub_period_by_stat))
  })
}

#test
base<- c(0:4, 3:1)
rev <- -base
sig <- rep(c(base, rev), 4)
period <- rep(1:4, each = 16 )
assertthat::are_equal(stat_per_period(sig, period)(max), rep(4,length(sig)))
assertthat::are_equal(stat_per_period(sig, period)(min), rep(-4,length(sig)))

set.seed(77)
sig_jit <- round(jitter(sig, amount = 3))
assertthat::are_equal(stat_per_period(sig_jit, period)(max), rep(c(-6, -5, -4, -7),each = 16))
assertthat::are_equal(round(stat_per_period(sig_jit, period)(mean)), rep(c(1, 0, 0, -1),each = 16))

#function that give the length of each period of the signal
period_id2duration <- function(periods_id) {
  ln <- length(periods_id)
  diffs <- periods_id[-ln] != periods_id[-1]
  idx <- c(which(diffs), ln )
  nb <- diff(c(0, idx[-ln]))
  rep(nb,nb)
}
period_id2duration(period)
assertthat::are_equal( period_id2duration(
    c(1,1,1,2,3,3,3,3)), 
    c(3,3,3,1,4,4,4,4))
assertthat::are_equal( period_id2duration(
    c(1,1,1,-2,-2,-2,-2,3,3,4,4,4,5)), 
    c(rep(3,3),rep(4,4),rep(2,2), rep(3,3), 1))

# function that substitutes each sequence of "above average" part of a signal by its max peak
# returns a vector of the same type as signal
min_max_signal <- function(signal, signal_avg) {
  #boolean vec :check if signal is above or under the loess
  above <- signal >= signal_avg
  #split into lists of the sequence alternatively above and under avg
  ##RMQ split(diff(above) != 0) <=> split where passing from above to under avg or vice-versa
  ##add a 0 elem bcz diff is  #cumsum to distinguish each above/under passage and each under/above passage
  sig_abv_undr <- signal %>% split(cumsum(c(0, diff(above) != 0)))
  #sequence of T/F corresponding to the signal above (True) or under (False)
  bool_seq_abv <- rle(above)$values
  #create of list of the above sequence and replace each ele of a seq by the max of the seq
  #ex : list(list(1, 2), list(-4, -1), list(-1, 1)) => list(list(2,2), list(-4, -1), list(-1, 1))
  sig_seq_abv <- sig_abv_undr[bool_seq_abv] %>% 
    purrr:map(function(lst) rep(max(lst),length(lst))) # map from purrr library
  
  sig_seq_undr <- sig_abv_undr[!bool_seq_abv] %>% 
    purrr:map(function(lst) rep(min(lst),length(lst))) 
  #replicate the max of the sequence above avg (the min if under)
 
  #put the max & min sequences together
  min_max_seq <- c(sig_seq_abv,sig_seq_undr) 
  #order them back ton their ordre bfr being split  
  min_max_seq_order <- min_max_seq[order(as.numeric(names(min_max_seq)))]
  
  unlist(min_max_seq_order)
}

##DEBUG
#data.AB <- data.AB[,1:7]
#data.AB$loess <- loess(data.AB$respiration ~ as.numeric(data.AB$date), degree=1,span=.1)$fitted
#data.AB$min_max_sig <- min_max_signal(data.AB$respiration, data.AB$loess)
#ggplot(data = data.AB[0:5000,]) + geom_line(aes(x= date, y = respiration), color = "blue") +
#  geom_smooth(method="loess", formula = y ~ x, se=TRUE, size= 1, span=.2, color = "green")  +
#  geom_line(aes(x = date, y = loess ), color = "green")  +
#  geom_line(aes(x = date, y = s_max_min), color = "red") 

##DEBUG
#series <- c (0,1,2,3,4,3,2,1)
#series.op <- - series
#repet <- 4
#my_signal <- rep(c(series, series.op),repet)
##fast loes for testing
#my_signal_loess <- rep(rep(0,2*length(series)), repet)
#expect_equal(length(my_signal_loess), length(my_signal))
#min_max_sig <- min_max_signal(my_signal, my_signal_loess)

#function that give the frequence of the signal
min_max_sig2freq <- function(min_max_signal) {
  diffs <- min_max_signal != lag(min_max_signal, default = min_max_signal[1])
  idx <- c(which(diffs), length(min_max_signal)+1)
  nb <- diff(c(1, idx))
  rep(nb,nb)
}
#min_max_sig2freq(min_max_sig)
#head(min_max_sig2freq(data.AB$min_max_sig))
#data.AB$freq_sig <- NULL
#data.AB$freq_sig <- min_max_sig2freq(data.AB$min_max_sig)

#ggplot(data.AB[1:100,], aes(date, respiration)) + geom_line(col="red") + 
#  geom_line(aes(y= min_max_sig), col="blue") +
#  geom_text(aes(y = min_max_sig, label= freq_sig), check_overlap = TRUE)
