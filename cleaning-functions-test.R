###############################################################
#################### CLEANING FUNCTIONS TESTS #################
###############################################################

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
#points(x, noise1, col="yellow", pch=20)

y.clean <- cleaning.high.freq(y.noise, 1/26)



data.dir.path <- get.data.path("occi")
expect_that(data.dir.path, equals("/Users/godot/githubRepos/occi/data/"))

file.path <- paste(data.dir.path,"data-test.csv",sep = "")

tbl.test <- csv2tbl.df(file.path, data.sep = "\t", dec.sep = ".")
expect_that(colnames(tbl.test), is_equivalent_to(c("traj_id","x","y","t","error")))
expect_equal(dim(tbl.test), c(17,5))
expect_equal(as.character(tbl.test[1,]),c("1","172.16","-29.97","1","0.68"))
expect_true(tbl.test[1,1]=="fe3ba46c-6ff2-4230-90df-337f8cafd112")


#format & add var tests
context("data modifications")



#debug
plot(x, y, type="l", col="red", ylim=range(-1.5*max_y,1.5*max_y,5))
points(x, y.clean, type="l", col="black") 
points(x, y.noise, col="green", pch=20)

