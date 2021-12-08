# Topic 8 - ANOVA

rm(list=ls())


data<-read.csv("C:/Data/tablets1.txt", sep = ",", quote = ' ', header = TRUE)

names(data) = c('lab1', 'lab2', 'lab3', 'lab4', 'lab5', 'lab6', 'lab7')

attach(data)
data

amount <- c(lab1, lab2, lab3, lab4, lab5, lab6, lab7)

lab<-c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10), rep(6,10),rep(7,10))

newdata<-data.frame(amount = amount,lab = lab)

attach(newdata)

newdata$lab = as.factor(newdata$lab)

newdata
#########
##########
anova<-aov(amount~lab, data = newdata)

summary(anova)



tapply(amount, lab, mean)

mean(amount)

tapply(amount, lab, mean) - mean(amount)

#####  CALCULATE F OF ANOVA MANUALLY:
I = 7
J = 10
SSB = J* sum( (tapply(amount, lab, mean) - mean(amount))^2 )

SSW = 0

for (i in 1:I) {
SSW = SSW + sum((amount[which(lab == paste(i))] - mean (amount[which(lab == paste(i))]) )^2) }
F = (SSB/(I - 1))/(SSW/(I*(J-1)))
F

####  KRUSKALL-WALLIS TEST

kruskal.test(amount~lab) 


##########  BONFERRONI TEST:

pairwise.t.test(amount, lab, p.adj = "bonf") # pool.SD = TRUE if variances are all equal



##########  TUKEY  TEST:

tukey<-TukeyHSD(anova, conf = 0.95)

tukey

#To find which couple are significantly different at a = 0.05:

which(tukey$lab[,4]<0.05)

########## Bonferroni for non-parametric case:

pairwise.wilcox.test(amount, lab, p.adjust.method = "bonf")


##############  LSD

MSW <- sum(anova$res^2)/(I*(J-1))
lsd <- qt(0.975,63)*sqrt(MSW*2/J)


###Function to check if the difference of 2 means > LSD
check.lsd <- function(data,i,j,lsd){ 
  mx <- mean(data[,i]); 
  my <- mean(data[,j])
  d <- mx - my
  if(abs(d)>lsd)
    cat("There is significant difference between groups",
          i,"&",j,"\n","Means =",mx,",",my," Diff =", d," > LSD =",lsd,"\n")
  else cat("There is no significant difference between groups",
             i,"&",j,"\n","Means =",mx,",",my," Diff =", d," < LSD =",lsd,"\n")}


check.lsd(data,1,4,lsd)

check.lsd(data,7,2,lsd)

#############   Equal Variances TEST:



#can use the Bartlett test (when samples are assumed normality):

res <- bartlett.test(amount ~ lab, data = newdata)
res


# Levene test:
install.packages("car")
library(car)

leveneTest(amount ~ lab, data = newdata)



##### normality checking  for the residuals:
#Komorgorov:

anova = aov(amount~lab, data = newdata)
resid=anova$res

ks.test(resid,"pnorm",mean(resid),sd(resid))

ks.test(amount,"pnorm",mean(amount),sd(amount))


#Shapiro Wilk Test (this is better)

shapiro.test(amount)

shapiro.test(resid)



############  Residuals vs Fitted:

resid=anova$res
fit = anova$fitted

plot(anova$fitted,anova$res, xlab="fitted", ylab= "Residuals", main = "")
abline(h=0)

############  QQ plot of Residuals :

qqnorm(anova$res,datax = TRUE, ylab = "Residuals", xlab = "Z scores", main = "QQ plot of Residuals")
qqline(anova$res,datax = TRUE)

