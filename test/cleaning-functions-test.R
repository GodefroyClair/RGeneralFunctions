##############################################################
################### CLEANING FUNCTIONS TESTS #################
##############################################################

#signal cleaning testing
context("signal")
source("../general-functions/cleaning-functions.R")

#create a signal of 2 cycles of 500 samples each
number_of_cycles = 2
max_y = 40

x = 1:500
a = number_of_cycles * 2*pi/length(x)

y = max_y * sin(x*a) #frequency : 1/500 * 2 = 1/250
noise1 = max_y * 1/10 * sin(x*a*10) #freq : 1/500 * 2 * 10 = 1/25
y.noise <- y + noise1

#debug
plot(x, y, type="l", col="red", ylim=range(-1.5*max_y,1.5*max_y,5))
points(x, y.noise, type="l", col="green")

y.clean <- cleaning.high.freq(y.noise, 1/26)

#debug
par(mfrow = c(1, 2))
plot(x, y, type="l", col="red", ylim=range(-1.5*max_y,1.5*max_y,5), ylab = "signal et noise")
points(x, y.noise, type="l", col="green")
plot(x, y.clean, type="l", col="black", ylim=range(-1.5*max_y,1.5*max_y,5), ylab = "signal filtré") 

par(mfrow = c(1, 1))
fft.y.noise <- spectrum.analysis(y.noise)
fft.y <- spectrum.analysis(y)
plot(Mod(fft.y.noise), type='l', main='Raw serie - fft spectrum')
plot(Mod(fft.y), type='l', main='Raw serie - fft spectrum')
