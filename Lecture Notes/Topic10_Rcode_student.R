####    TOPIC 10 - SIMULATION


set.seed(999)

##############  T-DISTRIBUTION  

N = 1000

n = 9

sample_mean = numeric(N)

sample_sd = numeric(N)

t = numeric(N) # vector length N of 0.



for (i in 1:N){ 

x = rnorm(n,0,2) # a sample of normal, size n, mean 0, sd = 2

sample_mean[i] = mean(x)

sample_sd[i] = sd(x)

t[i] = sample_mean[i]/(sample_sd[i]/sqrt(n) )

}
hist(t, breaks = 20,prob = TRUE, ylim= c(0,0.4), col = 5)

# OVERLAYING A T-DENSITY CURVE ON THE HISTOGRAM:
x <- seq(min(t), max(t), length.out=N)
y <- dt(x, (n-1) )
lines(x, y, col = "red",lty = 2)

# DRAW THE HISTOGRAM AND A T-DENSITY CURVE OVERLAYING USING GGPLOT2 PACKAGE:
library(ggplot2)
data = data.frame(t)
ggplot(data) + geom_histogram(aes(x = t),data = data,
              binwidth = 0.5, fill = "grey", color = "red")

p <- ggplot(data) +
    geom_histogram(aes(x = t, y = ..density..),
                   binwidth = 0.5, fill = "grey", color = "black")
x <- seq(min(t), max(t), length.out=N)
y = y <- dt(x, (n-1) )
df <- data.frame(x = x, y = y)
p + geom_line(data = df, aes(x = x, y = y), color = "red")





######################  EXAMPLE 2: BUFFON'S NEEDLE EXPERIMENT


# x ~ U(0,d/2), t ~ U(0,pi)
# where d is the distance between 2 parallel lines
# A needle of length L cut one of the lines if x < L/2*sin(t)
# Theoretical Probability = 2*L/(pi*d)

ns <- 500000 #NUMBER OF EXPERIMENTS
d <- 2
L <- 1
d2 <- d/2

# Theoretical answer
2*L/(pi*d)
#[1] 0.3183099

x <- runif(ns, 0, d2) # Generate a sample of size ns from uniform(0, d/2)
t <- runif(ns, 0, pi) # Generate a sample of size ns from uniform (0, pi)
length(x[x<L/2*sin(t)])/ns

[1] 0.31982




##### EXAMPLE 3: COMPARING ESTIMATORS of MU: TRIMMED MEAN VS WINSORIZED MEAN
# to see which estimator is better.
# consider the case when the population is highly skewed (underlying distribution is exponential distribution)
# to see which estimator is better. We compare MSE (estimater with smaller MSE is better)


set.seed(999)
rm(list=ls())

#define the winsorized mean:
winsor<-function(x, alpha = 0.2) # alpha = 0.2 or can be more
{ 
  n = length(x)
  xq = n * alpha
  
  x = sort(x)
  
  m = x[(round(xq)+1)]
  
  M = x[(n - round(xq))]
  
  x[which(x<m)] = m
  
  x[which(x>M)] = M
  
  return(mean(x)) 
} 

# UNDERLYING DISTRIBUTION = EXPONENTIAL WITH MEAN = 5000, var = 5000^2

T = 50
lambda = 5000

test = c()
for(j in 1:T) {
  
  N = 50
  n = 100
  
  Mean = rep(0,N)
  Win = rep(0,N)
  
  
  for (i in 1:N){ 
    
    x = rexp(n, rate = 1/lambda) # mean = lambda and var = lambda^2
    
    Mean[i] = mean(x, trimmed = 0.3) # trimmed mean 20% or more
    Win[i] = winsor(x, alpha = 0.3) # winsorized mean, alpha = 0.2 or can be more     
  }
  
  mse.mean = (mean(Mean) - lambda)^2 + var(Mean)# MSE
  mse.win = (mean(Win) - lambda)^2 + var(Win) # MSE
  
  test = append(test,isTRUE(mse.win < mse.mean) )# check if MSE of Winsorized mean is smaller than the MSE of trimmed mean
 
}

test # all = FALSE, meaning all the mse.win  are greater than mse.mean. 

# The winsorized mean has LARGER mse than the trimmed mean
# Hence, for skewed data, the trimmed mean MIGHT BE a better estimator of location than the winsorized mean.









####################  SIMULATION TO COMPARE SAMPLE MEAN, SAMPLE MEDIAN 
# AND SAMPLE 10% TRIMMED MEAN WHEN ESTIMATING POPULATION MEAN #########


# To compare 3 estimators for location, mu, through a simulation study
# The 3 estimators are sample mean, sample median, and 10% trimmed mean.

rm(list= ls())
M <- 10000 # simulation size (No. of samples)
n <- 20 # size of each sample

mu <- 170 # Mean of the underlying normal distribution
sd <- 10 # Standard deviation of the underlying normal distribution
meanx <- numeric(M) # A vector of all sample means
medx <- numeric(M) # A vector of all sample medians
trmx <- numeric(M) # A vector of all sample 10% trimmed means
stdx <- numeric(M) # A vector of all sample standard deviations

# Using the same seed number gives same result every time
set.seed(1234)

for (i in 1:M){
x <- rnorm(n,mu,sd) # Generate a random sample of size n
meanx[i] <- mean(x) # Compute the mean of the i-th sample: T^1_i
medx[i] <- median(x) # Compute the median of the i-th sample: T^2_i
trmx[i] <- mean(x, trim=0.1)
# Compute the 10% trimmed mean for the i-th sample, i.e. T^3_i
stdx[i] <- sd(x)
# Compute the standard deviation for the i-th sample.
# It is used for computing confidence interval.
}

# Compute mean of M sample means, medians, and trimmed means
simumean <- apply(cbind(meanx,medx,trmx),2,mean)

# "2" in the second argument asks R to find "means" for all the COLUMNS 
# in the matrix given in argument 1.
# Hence "simumean" is a 1 x 3 vector consists of
# the average of the M means, medians, and the 10% trimmed means.

# Compute sd of the M sample means, medians and 10% trimmed means
simustd <- apply(cbind(meanx,medx,trmx),2,sd)


#Compute the bias
simubias <- simumean - rep(mu,3)


# Compute the MSE (Mean Square Error)
simumse <- simubias^2 + simustd^2 


estimators <- c("Sample Mean", "Sample Median", "Sample 10% trimmed mean")
# column heading in the output
names <- c("True value", "No. of simu", "MC Mean",
"MC Std Deviation","MC Bias","MC MSE")


# row heading in the output
sumdat <- rbind(c(mu,mu,mu), M, simumean, simustd, simubias, simumse)


dimnames(sumdat) <- list(names,estimators)
round(sumdat,4)






################## COVERAGE PROBABILITY OF CONFIDENCE INTERVAL

# We make use of the data obtained from the above simulation study
# Get t_{0.025}(0.025)
t05 <- qt(0.975,n-1)
d <- 0
for (i in 1:M) {
# check if the i-th CI covers mu or not
cover <- (meanx[i]-t05*stdx[i]/sqrt(n)<= mu)&(mu<= meanx[i]+t05*stdx[i]/sqrt(n))
d <- d + cover
}
coverage <- d/M; coverage # this value should be close to 0.95

# The above statement is equivalent to
cover <- (meanx-t05*stdx/sqrt(n) <= mu) & (mu <= meanx+t05*stdx/sqrt(n))
coverage <- sum(cover)/M; coverage # this value should close to 0.95





################## CHECKING THE SIZE OF HYPOTHESIS TESTING
# We make use of the data obtained from the above simulation study
ttests <- (meanx-mu)/(stdx/sqrt(n))
size <- sum(abs(ttests) > t05)/M
size 

# value of size will change with the set.seed()
# if set.seed(1234) when simulating data then you will get size = 0.0462
# if set.seed(999) then you will get size = 0.0503



