View(tayotacorella)
library(e1071)
library(car)
colnames(tayotacorella)=c("price","age","km","hp","cc","doors","gears","quarterlytax","weight")
View(tayotacorella)
attach(tayotacorella)

# First Moment Business Decision
summary(tayotacorella1)
# second moment Business Decision
# Findout standard deviation 
sd(price)
sd(age)
sd(km)
sd(hp)
sd(cc)
sd(doors)
sd(gears)
sd(quarterlytax)
sd(weight)
var(Price)
# Find Out the variance(second business moment)
var(price)
var(age)
var(km)
var(hp)
var(cc)
var(age)
### skewness (third business momemt)
skewness(price)
skewness(hp)
## kurtosis (fourth Business decision)
kurtosis(price)
kurtosis(hp)
## scatter Plot for each Independent variable 
plot(age,price)
plot(km,price)
plot(hp,price)
plot(cc,price)
plot(doors,price)
plot(gears,price)
plot(quarterlytax,price)
plot(weight,price)

## Corelation Between the input & Output
pairs(tayotacorella)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(tayotacorella)

##Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(tayotacorella))

## Building linear regression model
model <- lm(price ~ ., data = tayotacorella)
summary(model) # Here P value For cc & doors are greater than 0.05
# cc and Doors are influence to each other, predict the model based on individual records
model.carcc <- lm(price ~ cc)
summary(model.carcc) # Its significat to output(p value individual less than 0.05)
model.cardoor <- lm(price ~ doors)
summary(model.cardoor) # It's also significatnt(p value individual less than 0.05)
## Build model with cc and Doors
model.car <- lm(price ~ cc + doors)
summary(model.car) # Both are significant to each other

# Find out the influencial record
influence.measures(model.car)
# ploting influential measures
influenceIndexPlot(model.car)
influencePlot(model.car) # in Cooks Distance 81 row is influencing so delete it

# Delete influentails records and build the model
model1 <- lm(price ~ ., data = tayotacorella[-c(81),])
summary(model1)

# Variance Influence factor 
vif(model1) # all the independent variables vif factor is less than 10

avPlots(model1)
# Final model ( doors independent variable is colinearity problem so delete it & predict the proce of the car )
finalmodel <- lm(price ~ age+ km + hp + cc + gears +quarterlytax + weight, data = tayotacorella[-c(81),])
summary(finalmodel)
## Now All the idependent variables there is no colinearity problem & P for all the independent variable less Than 0.05
## R^2 value is 86% 