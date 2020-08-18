computerdata=read.csv("computerdata")
head(computerdata)
attach(computerdata)
View(computerdata)


### Checking Data is normally dstributed or not by using Q Q Test####
qqnorm(hd)
qqline(hd)
qqnorm(price)
qqline(price)
qqnorm(speed)
qqline(speed)
hist(hd)
hist(speed)
hist(price)
## in data set some columns are discrete data so 
## we have to change it to contineous data by creating dummy variable 
install.packages("dummies")
library(dummies)
dummy[cd=="yes"]=1
computerdata.new=dummy.data.frame(computerdata)
dummy[cd=="yes"]=1
dummy[cd=="no"]=0
dummy[multi=="yes"]=1
dummy[multi=="no"]=0
dummy[premiun=="yes"]=1
dummy[premium=="no"]=0
dummy(computerdata)
View(computerdata.new)

## after creating dummy varibles in data set end created null values so 
## in order to delete that column below script applied 
test=data.frame(computerdata.new)
test2=test[,-14]
View(test2)
test3=test2[,-14]
View(test3)
test4=test3[,-14]
test5=test4[,-14]
test6=test5[,-14]
test7=test6[,-14]
View(test7)

## Explore the data##
plot(hd,price)
plot(ram,price)

pairs(test7)

cor(test7)# Corelation matrix
 ## multi linear model ###
model.cmp=lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(model.cmp) # P value for all the variables less than 0.05 only 
# R62 value also 77% so there is no colinearity problem

## Diagnostic plot##
install.packages("car")
library(car)
plot(model.cmp)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance


# Deletion Diagnostics for identifying influential variable
influence.measures(model.cmp)
influenceIndexPlot(model.cmp) # Index Plots of the influence measures
influencePlot(model.cmp)## here cooks graph also all the varibles less than 0.05 only so again there is coliniearity double confirmed 

vif(model.cmp) #vif>10 Coliniarity but here every variable vif value less than 10 only
# so here there is no linearity problem
avPlots(model.cmp,id.n=2,id.cex=0.7)

final.model=lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(final.model)
# p value for all the variables less than 0.05 & R^2  Value is 77% 

### Conclusion: if we wnt to predict the cost price of computer all the indepent variables(which is given in data set)has to take considertion,
                 #statistically proved by using multilinear model