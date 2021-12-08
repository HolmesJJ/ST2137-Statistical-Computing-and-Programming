rm(list=ls())
data<-read.table("Life_Expectancy_89countries.csv",sep = ',', header = TRUE)

names(data)

attach(data)

n = length(data$Country)

data

## PART I ########## LIFE EXPECTANCY  


# Q1
summary(Life_expectancy)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   51.7    65.5    74.0    72.1    77.6    89.0

range(Life_expectancy) # 51.7 89.0


quantile(Life_expectancy, prob = c(0.25, 0.5, 0.75))
# 25%  50%  75% 
# 65.5 74.0 77.6

mean(Life_expectancy) # 72.10112

sd(Life_expectancy) # 8.82604

# Not a must to use the code above.
# as long as student can get the required values.


# Q2
hist(Life_expectancy, freq = FALSE, col = 5)
x <- seq(50,90,length.out = length(Life_expectancy)) 
y <- dnorm(x,mean(Life_expectancy),sd(Life_expectancy))
lines(x,y, col = "red")

# can use different code to get this figure



# Q3
# 99% CI for the population  mean of life expectancy:
CI = c(mean(Life_expectancy) - qt(0.995, (n-1))*sd(Life_expectancy)/sqrt(n)  ,
       mean(Life_expectancy) + qt(0.995, (n-1))*sd(Life_expectancy)/sqrt(n) )

CI # 69.63793 74.56432

# t.test(Life_expectancy, mu = 0, conf.level = 0.99)
# this t.test also could provide 99% CI for mean.





#    PART II   #################   Life_expectancy AND Adult_mortality

# Q4
cor(Life_expectancy, Adult_mortality) #-0.7066861



# Q5

plot(Adult_mortality,Life_expectancy, type = "n")

points(Adult_mortality[which (Status=="Developed")],Life_expectancy[which (Status=="Developed")],pch = 10, col = 2)

points(Adult_mortality[which (Status=="Developing")],Life_expectancy[which (Status=="Developing")],pch = 20, col = "darkblue")

legend(300, 85, legend = c("Developed", "Developing"),pch = c(10, 20), col = c(2, "darkblue"))

# not a must to use the code above. As long as student could get the figure correctly
# comment on the figure:in general, there is a negative relationship between. 
# Life expectancy and Adult mortality. The relationship appears NOT obviously linear.
# For developed contries, the relationship is strongly negative and seems linear.
# For developing countries, it's not clearly linear.



#    PART III   #################   Life_expectancy AND Alcohol


# Q6

cor(Life_expectancy, Alcohol)  #0.554


# Q7
al = rep(0,n)

for (i in 1:n){
if (Alcohol[i]<=3) {al[i] = 1}
else if (Alcohol[i] <=6) {al[i] = 2}
else if(Alcohol[i] <=9) {al[i] = 3}
else {al[i] = 4}

}


# Q8
tab = table(Status, al)
tab
# 9 developed countries has al = 4
# 46 developing countries has al = 1




# Q9: 
prop.table(tab, "Status")

# they might be associated: developed countries tend to consume more alcohol
# (64.28% of developed countries has level al = 4 
# while developing countries tend to consume less alcohol 
# (61.3% of developing country has level of al = 1).
# possible association between al and Status: 
# more developed contries consumehigher level of alcohol


# Q10

# HYPOTHESES:
# Ho: two variables are independent
# H1: two variables are associated


nc1 = tab[1,] ## Column 1
nc2 = tab[2,] ## Column 2

# SCORES
v<-c(1,0);  ## specifying the scores for columns ( 1 = developed, 0 = developing)
u<-c(1,2,3,5) #higher scores for higher alcohol consumption



# TEST STATISTIC CALCULATION and NULL DISTRIBUTION

rsum<-nc1+nc2;  ## Row sums
csum<-c(sum(nc1),sum(nc2)); ## Column sums
n<-sum(csum)  ## total cell counts

rowp<-rsum/n  ## margin prob for rows
colp<-csum/n  ## margin prob for columns

pc1<-rsum*csum[1]/n;  ## prediction of Column 1
pc2<-rsum*csum[2]/n;  ## prediction of Column 2


ubar=sum(u*rowp);  ## weighted average scores for rows
vbar<-sum(v*colp);  ## weighted average scores for columns
CV<-sum(c(sum((u-ubar)*nc1/n),sum((u-ubar)*nc2/n))*(v-vbar))  ## weighted covariance

V1<-sum((u-ubar)^2*rsum/n); ## weighted variance for rows' scores
V2<-sum((v-vbar)^2*csum/n);  ## weighted variance for columns' scores

r<-CV/sqrt(V1*V2)  ## weighted correlation
r # 0.5652266

M<-sqrt(n-1)*r  ## Normalized test statistic
M # 5.302295

# TEST STATITSIC is M^2, which has CHI-SQUARE_1 DISTRIBUTION
M^2  #28.11434


# P-VALUE

#Test P-value (2 sided p-value)
1-pchisq(M^2,1)

# p-value  = 1.143556e-07 is very small. 

#CONCLUSION:

# Data provide very strong evidence against Ho. 
# That means, the status of a country and the level of alcohol might not be independent




############  ANOTHER WAY TO GET THE TEST IN Q10:


Input =(
"Status   Developed    Developing
al                       
1         3            46
2         0            12
3         2            14
4         9            3
")

set = as.table(read.ftable(textConnection(Input)))

set

library(coin)
test = lbl_test(set,scores = list(Status = c(1,0), al = c(1,2,3,5)))

test
