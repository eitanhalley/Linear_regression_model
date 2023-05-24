library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(readxl)
library(knitr)
library(rmarkdown)
library(simmer)
library(simmer.plot)
library(gmodels)
library(lattice)
library(hrbrthemes)
library(ggExtra)
library(rms)
library(strucchange)
library(leaps)
library(alr3)
library(HH)
library(car)
library(lmtest)

filePath=choose.files() 
projectData <- read.csv(filePath,header=TRUE)
dataNoExceptions1<-subset(projectData,projectData$sqft_lot<101930)
dataNoExceptions<-subset(dataNoExceptions1,dataNoExceptions1$bedrooms<6)

#2.1
#correlation and plots for continuous variables
cor.test(dataNoExceptions$sqft_living,dataNoExceptions$price,alternative = "two.sided",method = "pearson")
plot(dataNoExceptions$sqft_living,dataNoExceptions$price,xlab = "sqft_living",ylab = "price")

cor.test(dataNoExceptions$sqft_lot,dataNoExceptions$price,alternative = "two.sided",method = "pearson")
plot(dataNoExceptions$sqft_lot,dataNoExceptions$price,xlab = "sqft_lot",ylab = "price")

cor.test(dataNoExceptions$sqft_basement,dataNoExceptions$price,alternative = "two.sided",method = "pearson")
plot(dataNoExceptions$sqft_basement,dataNoExceptions$price,xlab = "sqft_basement",ylab = "price")

cor.test(dataNoExceptions$yr_built,dataNoExceptions$price,alternative = "two.sided",method = "pearson")
plot(dataNoExceptions$yr_built,dataNoExceptions$price,xlab = "yr_built",ylab = "price")

# partial F-test for categorical variables
#X2
Full.Model <- lm (dataNoExceptions$price ~  dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                    dataNoExceptions$sqft_basement +dataNoExceptions$floors + dataNoExceptions$condition + dataNoExceptions$yr_built + dataNoExceptions$renovated)
Reduced.Model.noBedroom <- lm(dataNoExceptions$price ~ dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                                dataNoExceptions$sqft_basement +dataNoExceptions$floors + dataNoExceptions$condition + dataNoExceptions$yr_built + dataNoExceptions$renovated)
anova(Reduced.Model.noBedroom, Full.Model)

#X6
Full.Model.noBedroom <- lm (dataNoExceptions$price ~ dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                    dataNoExceptions$sqft_basement +dataNoExceptions$floors + dataNoExceptions$condition + dataNoExceptions$yr_built + dataNoExceptions$renovated)
Reduced.Model.noBedroom.noFloors <- lm(dataNoExceptions$price ~ dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                                dataNoExceptions$sqft_basement + dataNoExceptions$condition + dataNoExceptions$yr_built + dataNoExceptions$renovated)
anova(Reduced.Model.noBedroom.noFloors, Full.Model.noBedroom)

#X7
Full.Model.noBedroom.noFloors <- lm (dataNoExceptions$price ~ dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                              dataNoExceptions$sqft_basement + dataNoExceptions$condition + dataNoExceptions$yr_built + dataNoExceptions$renovated)
Reduced.Model.noBedroom.noFloors.noCondition <- lm(dataNoExceptions$price ~ dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                                         dataNoExceptions$sqft_basement  + dataNoExceptions$yr_built + dataNoExceptions$renovated)
anova(Reduced.Model.noBedroom.noFloors.noCondition, Full.Model.noBedroom.noFloors)


#X9
Full.Model.noBedroom.noFloors.noCondition <- lm (dataNoExceptions$price ~ dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                                       dataNoExceptions$sqft_basement + dataNoExceptions$yr_built + dataNoExceptions$renovated)
Reduced.Model.noBedroom.noFloors.noCondition.noRenovated <- lm(dataNoExceptions$price ~ dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot + 
                                                     dataNoExceptions$sqft_basement  + dataNoExceptions$yr_built )
anova(Reduced.Model.noBedroom.noFloors.noCondition.noRenovated, Full.Model.noBedroom.noFloors.noCondition)

#2.2

install.packages("lsr")
library(lsr)
dataNoExceptions$yr_built_category <- case_when(
  dataNoExceptions$yr_built < 1980 ~ "old",
  dataNoExceptions$yr_built >= 1980 & dataNoExceptions$yr_built < 2000  ~ "average",
  dataNoExceptions$yr_built >= 2000 ~ "New"
)
#Create a new column of data
dataNoExceptions$yr_built_category <- factor(dataNoExceptions$yr_built_category)
plot(xlab="yr_built_category",ylab="price",dataNoExceptions$yr_built_category, dataNoExceptions$price)

#2.3
# we Suppose that variables bedrooms,floors and condition are categorical. 
# because their values can be considered as groups and not as continuous values.
model11<-lm(formula = dataNoExceptions$price ~ dataNoExceptions$bedrooms , data = dataNoExceptions)
summary(model11)
bedrooms_Deme <- factor(dataNoExceptions$bedrooms)
bedrooms_Deme <- relevel(bedrooms_Deme,ref = c(1))
model1<-lm(formula = dataNoExceptions$price ~ bedrooms_Deme , data = dataNoExceptions)
summary(model1)

model22<-lm(formula = dataNoExceptions$price ~ dataNoExceptions$floors , data = dataNoExceptions)
summary(model22)
floors_Deme <- factor(dataNoExceptions$floors)
floors_Deme <- relevel(floors_Deme,ref = c(1))
model2<-lm(formula = dataNoExceptions$price ~ floors_Deme , data = dataNoExceptions)
summary(model2)

model33<-lm(formula = dataNoExceptions$price ~ dataNoExceptions$condition , data = dataNoExceptions)
summary(model33)
condition_Deme <- factor(dataNoExceptions$condition)
condition_Deme <- relevel(condition_Deme,ref = c(1))
model3<-lm(formula = dataNoExceptions$price ~ condition_Deme , data = dataNoExceptions)
summary(model3)

#2.4

#Interaction varies between the number of floors and the size of the house
model10<-lm(formula = dataNoExceptions$price ~ dataNoExceptions$sqft_living * floors_Deme , data = dataNoExceptions)
summary(model10)
summary(dataNoExceptions$sqft_living)
plot(dataNoExceptions$sqft_living[floors_Deme=='1'], dataNoExceptions$price[floors_Deme=='1'],
     xlab="sqft_living", ylab="price", col="orange", xlim = c(770,5050), ylim = c(171800,2900000))
points(dataNoExceptions$sqft_living[floors_Deme=='2'], dataNoExceptions$price[floors_Deme=='2'], col="green")
points(dataNoExceptions$sqft_living[floors_Deme=='3'], dataNoExceptions$price[floors_Deme=='3'], col="red")
abline(a=(-35492.80) , b=(282.09) , col="orange") 
abline(a=(-35492.80+170126.88) , b=(282.09-91.09) , col="green") 
abline(a=(-35492.80+170126.88-483565.08) , b=(282.09-91.09+359.41) , col="red")

#Interaction varies between whether the property has been renovated and the sqft_living
model20<-lm(formula = dataNoExceptions$price ~ dataNoExceptions$sqft_living * renovated , data = dataNoExceptions)
summary(model20)
summary(dataNoExceptions$sqft_living)
summary(dataNoExceptions$price)
plot(dataNoExceptions$sqft_living[dataNoExceptions$renovated=='1'], dataNoExceptions$price[dataNoExceptions$renovated=='1'],
     xlab="sqft_living", ylab="price", col="orange", xlim = c(770,5050), ylim = c(171800,2900000))
points(dataNoExceptions$sqft_living[dataNoExceptions$renovated=='0'], dataNoExceptions$price[dataNoExceptions$renovated=='0'], col="green")
abline(a=(43342.13) , b=(230.17) , col="green") 
abline(a=(43342.13-574598.41) , b=(282.09+299.82) , col="orange") 


#Interaction varies between whether the property has been renovated and the yr_built
model30<-lm(formula = dataNoExceptions$price ~ dataNoExceptions$yr_built * renovated , data = dataNoExceptions)
summary(model30)
summary(dataNoExceptions$yr_built)
plot(dataNoExceptions$yr_built[dataNoExceptions$renovated=='1'], dataNoExceptions$price[dataNoExceptions$renovated=='1'],
     xlab="yr_built", ylab="price", col="orange", xlim = c(1990,2014), ylim = c(171800,2900000))
points(dataNoExceptions$yr_built[dataNoExceptions$renovated=='0'], dataNoExceptions$price[dataNoExceptions$renovated=='0'], col="green")
abline(a=(1.902e+06) , b=(-7.112e+02) , col="green") 
abline(a=(1.902e+06-3.987e+07) , b=(-7.112e+02+2.087e+04) , col="orange") 

#3.1

FullModel <- lm(dataNoExceptions$price ~floors_Deme + dataNoExceptions$yr_built
                + dataNoExceptions$bedrooms +
                + dataNoExceptions$sqft_living 
                + dataNoExceptions$sqft_lot
                + dataNoExceptions$sqft_basement
                + dataNoExceptions$renovated
                + dataNoExceptions$sqft_living*dataNoExceptions$renovated, data = dataNoExceptions)
EmptyModel <- lm(dataNoExceptions$price ~ 1, data = dataNoExceptions)

fwd.model <- step(EmptyModel, direction = "forward", scope= ~ floors_Deme + dataNoExceptions$yr_built
                  + dataNoExceptions$bedrooms +
                  + dataNoExceptions$sqft_living 
                  + dataNoExceptions$sqft_lot
                  + dataNoExceptions$sqft_basement
                  + dataNoExceptions$floors
                  + dataNoExceptions$renovated
                  + dataNoExceptions$sqft_living*dataNoExceptions$renovated)

bc.model <- step(FullModel, direction = "backward", scope= ~ 1)

bs.model <- step(EmptyModel, direction = "both", scope= ~ floors_Deme + dataNoExceptions$yr_built
                 + dataNoExceptions$bedrooms +
                 + dataNoExceptions$sqft_living
                 + dataNoExceptions$sqft_lot
                 + dataNoExceptions$sqft_basement
                 + dataNoExceptions$floors
                 + dataNoExceptions$renovated
                 + dataNoExceptions$sqft_living*dataNoExceptions$renovated)
BIC(bs.model)

BICfwd.model <- step(EmptyModel, direction = "forward",k = log(nrow(dataNoExceptions)), scope= ~ floors_Deme + dataNoExceptions$yr_built
                     + dataNoExceptions$bedrooms +
                     + dataNoExceptions$sqft_living
                     + dataNoExceptions$sqft_lot
                     + dataNoExceptions$sqft_basement
                     + dataNoExceptions$floors
                     + dataNoExceptions$renovated
                     + dataNoExceptions$sqft_living*dataNoExceptions$renovated)
BIC(BICfwd.model)

BICbc.model <- step(FullModel, direction = "backward",k = log(nrow(dataNoExceptions)), scope= ~ 1)
BIC(BICbc.model)

BICbs.model <- step(EmptyModel, direction = "both",k = log(nrow(dataNoExceptions)), scope= ~ floors_Deme + dataNoExceptions$yr_built
                    + dataNoExceptions$bedrooms +
                    + dataNoExceptions$sqft_living 
                    + dataNoExceptions$sqft_lot
                    + dataNoExceptions$sqft_basement
                    + dataNoExceptions$floors
                    + dataNoExceptions$renovated
                    + dataNoExceptions$sqft_living*dataNoExceptions$renovated)

BIC(BICbs.model)

summary(BICbs.model)
summary(bs.model)


#3.2

#the model we chose from the last part
model.02 <- lm(dataNoExceptions$price ~floors_Deme + dataNoExceptions$yr_built
          + dataNoExceptions$sqft_living 
          + dataNoExceptions$sqft_lot
          + dataNoExceptions$sqft_basement
          + dataNoExceptions$renovated
          + dataNoExceptions$sqft_living*dataNoExceptions$renovated, data = dataNoExceptions)
summary(model.02)

#linearity and variance equality check in plots
dataNoExceptions$fitted<-fitted(model.02) # predicted values
dataNoExceptions$residuals<-residuals(model.02) # residuals
s.e_res <- sqrt(var(dataNoExceptions$residuals))
dataNoExceptions$stan_residuals<-(residuals(model.02)/s.e_res)
plot(dataNoExceptions$fitted, dataNoExceptions$stan_residuals, xlab = "predicted value",
     ylab = "Normalized error")
abline(h = 0)

# Normality tests and plots
qqnorm(dataNoExceptions$stan_residuals)
abline(a=0, b=1)
hist(dataNoExceptions$stan_residuals, xlab ="Normalized error", main="Histogram of normalized error")

# Normality tests
ks.test(x= dataNoExceptions$stan_residuals,y="pnorm",alternative = "two.sided", exact = NULL)
shapiro.test(dataNoExceptions$stan_residuals)

# linearity test
sctest(model.02, type = "Chow")

# suspicious variable
var(dataNoExceptions$price[dataNoExceptions$sqft_basement <= 100])
var(dataNoExceptions$price[dataNoExceptions$sqft_basement  <= 500 & dataNoExceptions$sqft_basement > 100])
var(dataNoExceptions$price[dataNoExceptions$sqft_basement <= 1000 & dataNoExceptions$sqft_basement > 500])
var(dataNoExceptions$price[ dataNoExceptions$sqft_basement > 1000])

# try this one too
plot(dataNoExceptions$sqft_basement, dataNoExceptions$price)
lm(dataNoExceptions$price~dataNoExceptions$sqft_basement, dataNoExceptions)
abline(lm(dataNoExceptions$price~dataNoExceptions$sqft_basement, dataNoExceptions))

gqtest(dataNoExceptions$price ~floors_Deme + dataNoExceptions$yr_built
       + dataNoExceptions$sqft_living 
       + dataNoExceptions$sqft_lot
       + dataNoExceptions$sqft_basement
       + dataNoExceptions$renovated
       + dataNoExceptions$sqft_living*dataNoExceptions$renovated, order.by = dataNoExceptions$sqft_basement,data = dataNoExceptions,
       fraction =  nrow(dataNoExceptions)/6)

#4
#how to improve the model

#log(y)
dataNoExceptions$price_log <- log(dataNoExceptions$price)
var(dataNoExceptions$price_log[dataNoExceptions$sqft_basement <= 100])
var(dataNoExceptions$price_log[dataNoExceptions$sqft_basement  <= 500 & dataNoExceptions$sqft_basement > 100])
var(dataNoExceptions$price_log[dataNoExceptions$sqft_basement <= 1000 & dataNoExceptions$sqft_basement > 500])
var(dataNoExceptions$price_log[ dataNoExceptions$sqft_basement > 1000])

#(y)^0.5
dataNoExceptions$price_sqrt <- (dataNoExceptions$price)^0.5
var(dataNoExceptions$price_sqrt[dataNoExceptions$sqft_basement <= 100])
var(dataNoExceptions$price_sqrt[dataNoExceptions$sqft_basement  <= 500 & dataNoExceptions$sqft_basement > 100])
var(dataNoExceptions$price_sqrt[dataNoExceptions$sqft_basement <= 1000 & dataNoExceptions$sqft_basement > 500])
var(dataNoExceptions$price_sqrt[ dataNoExceptions$sqft_basement > 1000])

#(y)^0.5
model_sqrt <- lm(dataNoExceptions$price_sqrt ~ floors_Deme + dataNoExceptions$yr_built
               + dataNoExceptions$sqft_living 
               + dataNoExceptions$sqft_lot
               + dataNoExceptions$sqft_basement
               + dataNoExceptions$renovated
               + dataNoExceptions$sqft_living*dataNoExceptions$renovated, data = dataNoExceptions)
#log(y)
model_log <- lm(dataNoExceptions$price_log ~ floors_Deme + dataNoExceptions$yr_built
                + dataNoExceptions$sqft_living 
                + dataNoExceptions$sqft_lot
                + dataNoExceptions$sqft_basement
                + dataNoExceptions$renovated
                + dataNoExceptions$sqft_living*dataNoExceptions$renovated, data = dataNoExceptions)

#check if there still is a linear link
cor(dataNoExceptions$price_log, dataNoExceptions$sqft_living)
cor(dataNoExceptions$price_log, dataNoExceptions$sqft_lot)
cor(dataNoExceptions$price_log, dataNoExceptions$sqft_basement)
cor(dataNoExceptions$price_log, dataNoExceptions$yr_built)

# new var and empty model
dataNoExceptions$log_beedrooms <- log(dataNoExceptions$bedrooms)
Empty_log_model <- lm(dataNoExceptions$price_log ~ 1)


#check what the best model is by adding var and by removing var with 
LogModel <- step(model_log, direction = "both", 
                        scope = (~ floors_Deme + dataNoExceptions$yr_built + dataNoExceptions$log_beedrooms
                                 + dataNoExceptions$sqft_living 
                                 + dataNoExceptions$sqft_lot
                                 + dataNoExceptions$sqft_basement
                                 + dataNoExceptions$renovated
                                 + dataNoExceptions$sqft_living*dataNoExceptions$renovated))

LogModel <- step(Empty_log_model, direction = "both", 
                        scope = (~ floors_Deme + dataNoExceptions$yr_built + dataNoExceptions$log_beedrooms
                                 + dataNoExceptions$sqft_living 
                                 + dataNoExceptions$sqft_lot
                                 + dataNoExceptions$sqft_basement
                                 + dataNoExceptions$renovated
                                 + dataNoExceptions$sqft_living*dataNoExceptions$renovated))

#new model
full_log_model <- lm(dataNoExceptions$price_log ~ floors_Deme + dataNoExceptions$yr_built + 
                       dataNoExceptions$sqft_living + dataNoExceptions$sqft_lot)

summary(full_log_model)


#check the linearity and variance equality hypothesis
dataNoExceptions$fitted<-fitted(full_log_model) # predicted values
dataNoExceptions$residuals<-residuals(full_log_model) # residuals
s.e_res <- sqrt(var(dataNoExceptions$residuals))
dataNoExceptions$stan_residuals<-(residuals(full_log_model)/s.e_res)
plot(dataNoExceptions$fitted, dataNoExceptions$stan_residuals, xlab = "predicted value",
     ylab = "Normalized error")
abline(h = 0)

#check the Normality hypothesis
qqnorm(dataNoExceptions$stan_residuals)
abline(a=0, b=1)
hist(dataNoExceptions$stan_residuals, xlab ="Normalized error", main="Histogram of normalized error")

# Shapiro-Wilk and Kolmogrov-Smirnov tset
ks.test(x= dataNoExceptions$stan_residuals,y="pnorm",alternative = "two.sided", exact = NULL)
shapiro.test(dataNoExceptions$stan_residuals)

#check linear hypothesis
sctest(full_log_model, type = "Chow")


# suspicious variable
var(dataNoExceptions$price_log[dataNoExceptions$sqft_living <= 1500])
var(dataNoExceptions$price_log[dataNoExceptions$sqft_living  <= 2500 & dataNoExceptions$sqft_living> 1500])
var(dataNoExceptions$price_log[dataNoExceptions$sqft_living <= 3500 & dataNoExceptions$sqft_living > 2500])
var(dataNoExceptions$price_log[dataNoExceptions$sqft_living > 3500])

#Goldfeld Quandt test 
gqtest(full_log_model, order.by = dataNoExceptions$sqft_living,data = dataNoExceptions,
       fraction =  nrow(dataNoExceptions)/6)

