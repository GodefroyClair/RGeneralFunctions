###############################################################
#################### CLEANING FUNCTIONS #######################
###############################################################

#libraries necessary :
library(stats)
library(signal)

################ Cleaning signal ################
#usefull link :
#http://stackoverflow.com/questions/7105962/how-do-i-run-a-high-pass-or-low-pass-filter-on-data-points-in-r

##Low pass filter
##used with respiration data in affective computing project
#the idea is to clean all the "noise"
cleaning.high.freq <- function(df.col, threshold){
 low.pass.filter <- butter(2, threshold, "low") 
 signal::filtfilt(low.pass.filter,df.col) #filtfilt is used instead of filter to get rid of signal shift
}

#get the real spectrum analysis of a signal
fft.analysis <- function(signal){
  Mod(fft(signal)) 
}

spectrum.analysis <- function(signal, date){
  ## Calculate fft(ecg):
  fft <- fft(signal)
  ## Find the sample period:
  delta <- as.numeric(date[2] - date[1])
  
  ## Calculate the Nyquist frequency:
  f.Nyquist <- 1 / 2 / delta
  print(f.Nyquist)
  ## Calculate the frequencies.  (Since date smalest scale is in seconds, delta
  ## is in seconds, f.Nyquist is in kHz and freq is in kHz)
  ## (Note: I may be off by 1 in indexing here ????)
  f.Nyquist*c(seq(nrow(signal)/2), -rev(seq(nrow(date)/2)))/(nrow(date)/2)
}
spectrum.analysis(data.DE$respiration,data.DE$date)
