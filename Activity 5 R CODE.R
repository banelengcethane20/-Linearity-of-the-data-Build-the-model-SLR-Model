#PRACTICAL 
#(i)
# Linearity of the data 
library(ggfortify)#For graphics 
#Build the model (SLR Model) 
View(Foot)
attach(View(Foot))
model1 <- lm(foot ~ height, data = Foot) 
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 1) # For the Residual Vs Fitted Values

#Required packages
library(lmtest)#For the studentized Breusch-Pagan test
model1 <- lm(foot ~ height, data = Foot)
bptest(model1)

#Homogeneity of variance 
library(ggfortify)#For graphics 
library(datarium)#For the data
#Build the model (SLR Model) 
model1 <- lm(foot ~ height, data = Foot)
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 3) # For the Scale Location plot

# Null Hypothesis: Variances are equal
#Required packages 
library(lmtest)#For the studentized Breusch-Pagan test 
library(datarium)#For the data 
model1 <- lm(foot ~ height, data = Foot) 
bptest(model1)

# reduce the heteroscedasticity 
library(lmtest)#For the studentized Breusch-Pagan test 
model1_log <- lm(log(foot) ~ height, data = Foot) 
plot(model1_log, 3)

#  a Breusch Pagan Test to check for heteroskedasticity more formally
#Required packages 
library(lmtest)#For the studentized Breusch-Pagan test 
library(datarium)#For the data 
model1_log <- lm(log(foot) ~ height, data = Foot) 
bptest(model1_log)
#Here the null hypothesis is homoskedasticity (p > 0.05), and we fail to reject it.

#Normality of residuals 
library(ggfortify)#For graphics 
#Build the model (SLR Model) 
model1 <- lm(foot ~ height, data = Foot)
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 2) # For the Q-Q plot

library(car)
model1 <- lm(foot ~ height, data = Foot) 
qqPlot(model1, main="QQ Plot") #qq plot for studentized resid

#Null Hypothesis: Residuals are consistent with a normal distribution
library(stats);
library(nortest)#For the SW & AD tests 
library(datarium)#For the data 
model1 <- lm(foot ~ height, data = Foot) 
shapiro.test(model1$residuals) 
ad.test(model1$residuals)

#the skewness and kurtosis tests 
library(moments)#For the tests 
model1 <- lm(foot ~ height, data =Foot) 
skewness(model1$residuals) 
kurtosis(model1$residuals)



#Independency of residuals 
library(car)
model1 <- lm(foot ~ height, data = Foot) 
durbinWatsonTest(model1)
#Here the probability value is grater than 0.05 so we accept null hypothesis saying that there is no correlation among residuals(i.e residuals are independent). Therefore INDEPENDENCY IS MET on residuals.

#Outliers and high leverage points 
library(ggfortify)#For graphics 
#Build the model (SLR Model) 
model1 <- lm(foot ~ height, data = Foot)
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 5)


# Studentized Residuals vs Leverage plot. Using the Foot data
library(car)
model1 <- lm(foot ~ height, data = Foot)
leveragePlots(model1) # leverage plots

# Outliers 
library(car)
model1 <- lm(foot ~ height, data = Foot)
# Assessing Outliers
outlierTest(model1) 


#(ii)
#Outliers and high leverage points
library(ggfortify)#For graphics 
#Build the model (SLR Model) 
model1 <- lm(foot ~ height, data = Foot)
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 5)

#(iii)
# Studentized Residuals vs Leverage plot. Using the Foot data:

model1 <- lm(foot ~ height, data = Foot)
leveragePlots(model1) # leverage plots

# Outliers

model1 <- lm(foot ~ height, data = Foot)
# Assessing Outliers
outlierTest(model1) 


#(iv)
# Linearity of the data 
library(ggfortify)#For graphics 
library(datarium)#For the data
#Build the model (SLR Model) 
View(Infectionrisk)
attach(View(Infectionrisk))
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk) 
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 1) # For the Residual Vs Fitted Values

#Required packages
library(lmtest)#For the studentized Breusch-Pagan test
library(datarium)#For the data
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk)
bptest(model1)

# Homogeneity of variance
library(ggfortify)#For graphics 
library(datarium)#For the data
#Load the data 
#3data("Foot", package = "datarium") 
#Build the model (SLR Model) 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk)
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 3) # For the Scale Location plot

# Null Hypothesis: Variances are equal
#Required packages 
library(lmtest)#For the studentized Breusch-Pagan test 
library(datarium)#For the data 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk) 
bptest(model1)

# reduce the heteroscedasticity problem 
library(lmtest)#For the studentized Breusch-Pagan test 
model1_log <- lm(log(InfctRsk) ~ Stay, data = Infectionrisk) 
plot(model1_log, 3)

#  a Breusch Pagan Test to check for heteroskedasticity more formally.
#Required packages 
library(lmtest)#For the studentized Breusch-Pagan test 
library(datarium)#For the data 
model1_log <- lm(log(InfctRsk) ~ Stay, data = Infectionrisk) 
bptest(model1_log)
#Here the null hypothesis is homoskedasticity (p > 0.05), and we fail to reject it.

# Normality of residuals 
library(ggfortify)#For graphics 
#Build the model (SLR Model) 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk)
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 2) # For the Q-Q plot


library(datarium)#For plots and data 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk) 
qqPlot(model1, main="QQ Plot") #qq plot for studentized resid

#Null Hypothesis: Residuals are consistent with a normal distribution
library(stats);
library(nortest)#For the SW & AD tests 
library(datarium)#For the data 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk) 
shapiro.test(model1$residuals) 
ad.test(model1$residuals)

#  the skewness and kurtosis tests
library(moments)#For the tests 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk) 
skewness(model1$residuals) 
kurtosis(model1$residuals)

#Independency of residuals 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk) 
durbinWatsonTest(model1)
#Here the probability value is grater than 0.05 so we accept null hypothesis saying that there is no correlation among residuals(i.e residuals are independent). Therefore INDEPENDENCY IS MET on residuals.

# Outliers and high leverage points 
library(ggfortify)#For graphics 
#Build the model (SLR Model) 
model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk)
#Plot the diagnostic plots
par(mfrow = c(2, 2)) #Format of appearance 
plot(model1, 5)


# Studentized Residuals vs Leverage plot. Using the Foot data:

model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk)
leveragePlots(model1) # leverage plots

# Outliers 

model1 <- lm(InfctRsk ~ Stay, data = Infectionrisk)
# Assessing Outliers
outlierTest(model1) 








