
#R code for Topic 7 - Tests


#############     ONE SAMPLE DATA     ###########3


data = read.csv("C:/Data/babyweights.csv", header = TRUE)
attach(data)
summary(weight)
var(weight)


# t-test (parametric test) for comparing mean with a number:

t.test(weight, mu = 3.3,alternative = "less")

# Sign test (nonparametric test) for comparing median vs a number:

weight.non.0 = (weight[weight!=3.3])
binom.test(sum(weight<3.3), length(weight.non.0), alternative = "less") 
# OR: binom.test(x = c(sum(weight<3.3), sum(weight>3.3)), alternative = "less")


#Wilcoxon Signed Rank Test (also a nonparametric test) for comparing median vs a number:

wilcox.test(weight.non.0, mu=3.3, alternative="less")




#############     TWO-SAMPLE DATA     ###########

rm(list=ls())


data = read.csv("C:/Data/protein_and_weight_gain.csv", header = TRUE)
data

#names(data)

#data$level = factor(level)

attach(data)

x = weight_gain[which(level == "high")]

y = weight_gain[which(level == "low")]


mean(x)

mean(y)

var.test(x,y) # test if the variances are equal (the null: variances are equal)

#equivalently, can use the Bartlett test (when samples are assumed normality):

res <- bartlett.test(weight_gain ~ level, data = data)
res

# t-test (parametric test) to compare means:

t.test(x,y, mu = 0, var.equal = TRUE) # if variances are equal

t.test(x,y, mu = 0, var.equal = FALSE) # if variances are NOT equal



#Equivalently, can use the command below:

t.test(weight_gain~level, mu = 0, var.equal = TRUE)




###Mann Whitney U test in R

bf = c(87,96,92,84) # with breakfast

no.bf = c(93,83,79,73) #without breakfast

wilcox.test(bf,no.bf)


lotion = c(1,2,5)
studio = c(3,4)
wilcox.test(lotion, studio)


#for weight gain data:
wilcox.test(x,y)







#####################   PAIRED SAMPLES



before = c(25, 25,27, 44,30,67, 53, 53,52, 60, 28)

after = c(27, 29, 37, 56, 46, 82, 57, 80,61,59,43)

t.test(after, before, mu = 0, paired = TRUE, conf.level = 0.9) # t-test


#### nonparametric test for paired samples:

drugA = c(20, 40, 30, 45, 19, 27, 32, 26)
drugB = c(18, 36, 32, 46, 15, 22, 29, 25)

diff = drugA - drugB

ncount <-sum(sign(diff[diff>0]))# the number of positive signs

# Sign test:
binom.test(ncount, length(diff),0.5)


# Wilcoxon Signed Rank Test:
wilcox.test(diff)











