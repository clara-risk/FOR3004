
#############################################################################
#FOR3004 - Tutorial 8 Distributions
#############################################################################

#First, we will fit a Poisson distribution to a dataset (number fires per day), given here: 
library(MASS)
num_pd_R <-  c(35, 31, 17, 20, 22, 13, 19, 28, 26, 15, 8, 18, 48, 32, 28, 29, 33, 26, 27, 
               23, 27, 26, 21, 22, 30, 33, 20, 11, 15, 42, 45, 25, 15, 13, 19, 22, 28, 29, 
               17, 25, 20, 14, 17, 28, 14, 42, 37, 59, 50, 34, 33, 38, 59, 98, 35, 24, 51, 
               28, 19, 26, 17, 37, 41, 32, 40, 74, 23, 26, 16, 15, 22, 19, 22, 15, 20, 23, 
               10, 14, 18, 14, 18, 23, 12, 9, 10, 23, 12, 15, 17, 7, 3, 10, 12)

pmf<-fitdistr(num_pd_R, densfun="poisson") #Fit the Poisson distribution
print(pmf[1]) #Get the Poisson parameter which is the mean 

#Now we will visualize the fitted distribution using the parameter (lambda). 
eq <- function(x){dpois(x, lambda=25.56989)*100} #dpois gives us the Poisson function
curve(eq, from=0, to=100, xlab="Number Fires", ylab="Probability")

#Now, we will use the pmf (probability mass function) to determine the % chance of X number of fires tomorrow. 

calc_pmf <- function(x){dpois(x, lambda=25.56989)*100}

print(calc_pmf(10)) #Probability of 10 fires
print(calc_pmf(15))
print(calc_pmf(20))

#Let's create the cumulative distribution function (cdf) in R.
calc_cdf <- function(x){ppois(x, lambda=25.56989)*100}

print(calc_cdf(10)) # Chance of <= 10 fires
print(calc_cdf(15))
print(calc_cdf(20))

#How do we calculate the most likely number of fires tomorrow in R?

eq <- function(x){dpois(x, lambda=25.56989)*100}
test_num <- seq(from = 0, to = 100, by = 1)
initialize <- seq(0,0,length.out=101)
for(i in test_num){
  y <- eq(i)
  initialize[i] = y
}
print(max(initialize))
print(which(initialize == max(initialize)))

#How to generate a pseudo-random number in R
floor(runif(1, min=0, max=101))

#Using R to simulate the number of fires on a given day.
#Here we are sampling numbers from the fitted Poisson distribution. 
r_fire_num = rpois(1000,25.56989)

#Visualize the sampled numbers
hist(r_fire_num, breaks = 15)

#Now we will fit a continuous distribution. 

#Here is our data of area burned for June-July-August for Ontario for every year in the CNFDB
area_list_r <- c(2.39, 90.09, 97.43, 51.22, 16.03, 18.99, 14.57, 47.04, 3.04, 3083.94, 56.65, 
                 1272.2350002794656, 288.2340000357767, 9.494000024154134, 178.07500009323593, 
                 52.79300001715081, 1565.6700001368652, 18.06500006179487, 3713.2090002170617, 
                 955.4990000479664, 2.280000026076287, 41.85300000138578, 39.3610000194005, 
                 1128.601000048814, 2944.4760000331235, 1730.253000027834, 1568.394000038979, 
                 7.017000003605922, 3.1200000090893947, 36.38300000259266, 2221.6100000178208, 
                 231.2430000115927, 126.96800003044174, 709.5690000281469, 216.67900001212908, 
                 47.55700000874667, 48.81500002747593, 1271.5990000246304, 89.26400001324677, 
                 2.1910000047084037, 265.86100001181507, 1373.2470000674552, 44.85900002169566,
                 9.300000002622403, 0.49200000330792065, 14.687000005543162, 4734.622000031454, 
                 996.4670000256136, 267.57999999828604, 25.170000003203494, 58.51600001543674, 
                 15.444000019058272, 914.9369999852763, 1844.386000055223, 86.97000001206939, 
                 128.69100000000003)
#Fit the continuous exponential distribution. 
fitted_parameters <- fitdistr(area_list_r, "exponential") 
print(fitted_parameters[1])

#Calculate the parameter, which is the mean
mean <- 1/(0.001609976)
print(mean)

#Let's sample 1000 values from the fitted exponential distribution
area_burned <- rexp(1000, 1/621.1273)

#Visualize histogram 
hist(area_burned)


