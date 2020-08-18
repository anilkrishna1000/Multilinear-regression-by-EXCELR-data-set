
View(startup_50)
install.packages("data.table")
library(data.table)
summary(startup_50)
# find out the variance of each department
var(startup_50$R.D.Spend)
var(startup_50$Administration)
var(startup_50$Marketing.Spend)
var(startup_50$Profit)
# find out the standard deviation 
sd(startup_50$R.D.Spend)
sd(startup_50$Administration)
sd(startup_50$Marketing.Spend)
sd(startup_50$Profit)


unique(state) # Checking How Many city are in state 
startup_50 <- cbind(startup_50,ifelse(startup_50$State=="New York",1,0), ifelse(startup_50$State=="California",1,0),  ifelse(startup_50$State=="Florida",1,0))


# Renaming the column
setnames(startup_50, 'V2','New York')
setnames(startup_50, 'V3','California')
setnames(startup_50, 'V4','Florida')

# Ploting the data on scatter plot
plot(startup_50) # In this plot we are plotting dummy which seems no relative

## removing the State Column ###
test=data.frame(startup_50)
test1=test[,-4]
View(test1)
plot(test1)# After removing the state Column see the plot 

## after seeing scatter Finding the Correlation##
library(corpcor)
cor2pcor(cor(test1))
 
## Creating Model##
colnames(startup_50)

Profit_Model <- lm(Profit~`R.D.Spend`+Administration+`Marketing.Spend`, data = startup_50)

summary(Profit_Model) # P value for administration & Marketing.spend more Than 0.05
## so   check the Influence records

library(car)
## Loading required package: carData
influenceIndexPlot(Profit_Model)
influencePlot(Profit_Model,id.n=3) ## Here cooks Graphs also P value is More tan 0.05 So Double Confirmed 
## The Influence rows are 50 & 49 which is seeing in Cooks Graph

Profit_Model_Inf <- lm(Profit~`R.D.Spend`+`Administration`+`Marketing.Spend`, data = startup_50[-c(50,49),])

summary(Profit_Model_Inf)

## Variance influence factor to Check the Coliniarity Between the Variables

Profit_Model <- lm(Profit~`R&D Spend`+Administration+`Marketing Spend`, data = startup_50)
class(startup_50$`Marketing Spend`)

vif(Profit_Model)
summary(Profit_Model)
## vif>10 then there exists collinearity among all the variables 
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Profit_Model)

## Creating final Model for Administration data
Profit_Model_Revised <- lm(Profit~`R.D.Spend`+Administration+`Marketing.Spend`+`State`, data = startup_50)

library(MASS)

stepAIC(Profit_Model_Revised)

Profit_Model_Final <- lm(Profit~`R.D.Spend`+`Marketing.Spend`, data = startup_50)

summary(Profit_Model_Final)# R^2 value is 95% So Our Model is too sufficient and P value is also less than 0.05
### Here Administration variable  coliniarity to the marketing.spend so ignore that as the input variable 
## consider only independent variable as R.D. spend and Marketing.spend 
### Conclusion: if we want to Predict the Profit for 50 startup the independent variables consideration is R.D.Spend and Marketing.Spend