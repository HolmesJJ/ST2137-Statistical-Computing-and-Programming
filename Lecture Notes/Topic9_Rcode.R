# Topic 9: Regression Analysis


###################   Example 1: weight, height, age, gender

data = read.table ("C:/Data/ex10_1.txt" ,header=T)
data
attach(data)

cor(cbind(height, weight, age), method="pearson")

cor.test(height, weight)


#scatter plot of height and weight

plot(height, weight)


model1 <- lm(weight ~ height, data = data)
summary(model1)
anova(model1)



model2 <- lm(weight ~ height +age, data = data)
summary(model2)
anova(model2)


plot(weight, height, type = "n")
points(weight[gender =="M"], height[gender =="M"], col = "red", pch = 20)
points(weight[gender =="F"], height[gender =="F"], col = "blue", pch = 20)


# Add gender into regression model
data$gender = as.factor(data$gender)

model3 <- lm(weight ~ height +age + gender, data = data)
summary(model3) 

#Model with interaction term
model4 <- lm(weight ~ height +age + gender + height*gender)
summary(model4)
anova(model4)



#the raw residuals of model1
model1$res

#standardized residuals:
rstandard(model1)

#QQ plot of SR:
#this has theoretical quantile in Y-axis and observed quantiles in X-axis
qqnorm(rstandard(model1),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "QQ Plot", pch = 20)
qqline(rstandard(model1),datax = TRUE, col = "red")

#this has theoretical quantile in X-axis and observed quantiles in Y-axis
qqnorm(rstandard(model1), ylab = "Standardized Residuals", xlab = "Z scores", main = "QQ Plot", pch = 20)
qqline(rstandard(model1), col = "red")



#standardized residuals vs fitted:
plot(model1$fitted.values,rstandard(model1), xlab="fitted weight", ylab= "Standardized Residuals", main = "SR vs Fitted", pch = 20)
abline(h=0, col = "red")


#fitted values:
model1$fitted.values



### prediction of NEW OBSEVARIONS using model1:

# fitted values and CIs when (height = 65) and (height = 63)
predict(model1, newdata=data.frame(height=c(65,63)), interval = "confidence",level = 0.95)




### prediction using model2:

# fitted values and CIs when (height = 65, age = 40) and (height = 63, age = 36)
predict(model2, newdata=data.frame(height=c(65,63), age = c(40,36)), interval = "confidence",level = 0.95)


#Leverage
p = 2 # simple model weight ~ height has p = 2
n = length(weight)

x<-cbind(c(rep(1,n)),height) 
hat<-x%*%solve(t(x)%*%x)%*%t(x) # hat matrix
diag(hat)

2*p/n 
which(diag(hat)>2*p/n)
diag(hat)[which(diag(hat)>2*p/n)]


# Cook's distance

cooks.distance(model1)



# add height^2 to model1
model5 = model1 <- lm(weight ~ height + I(height^2), data = data)

#QQ plot of SR of model5

qqnorm(rstandard(model5),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "QQ Plot", pch = 20)
qqline(rstandard(model5),datax = TRUE, col = "red")

#standardized residuals vs fitted of model5
plot(model5$fitted.values,rstandard(model5), xlab="fitted weight", ylab= "Standardized Residuals", main = "SR vs Fitted", pch = 20)
abline(h=0, col = "red")










