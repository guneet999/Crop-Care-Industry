#Importing libraries:
library(readxl)
library(readr)
library(forecast)
library(smooth)
library(XML)
library(ie2misc)
library(timeSeries)
library(fpp)
library(dummies)

#Importing the Dataset:
forecasting <- read.csv("C://Users//HP//Downloads//forecastfinaldata.csv")
View(forecasting)
forecasting <- forecasting[c(-1)]
View(forecasting)
plot(forecasting$Value,type="o")
class(forecasting)
forecasting1 <- ts(forecasting$Value)
abline(reg = lm(forecasting1~time(forecasting1)))
plot(log(forecasting$Value),type="o")
plot(diff(log(forecasting$Value)),type="o")
plot(diff(diff(log(forecasting$Value))),type="o")
plot(diff(diff(diff(log(forecasting$Value)))),type="o")
plot(diff(diff(diff(diff(log(forecasting$Value))))),type="o")
plot(diff(diff(diff(diff(diff(log(forecasting$Value)))))),type="o")
plot(diff(diff(diff(diff(diff(diff(log(forecasting$Value))))))),type="o")
#Importing the Dummy data in field for months
dummy2 <- as.data.frame(outer(rep(month.abb,length=24),month.abb,"==")+0)
colnames(dummy2) <- month.abb
forecasting <- cbind(forecasting,dummy2)
forecasting["t"] <- 1:24
forecasting["tsquare"] <- (forecasting$t)*(forecasting$t)
forecasting["log_value"] <- log(forecasting$Value)
#Spliting the data
train <- forecasting[1:12,]
test <- forecasting[13:24,]

attach(forecasting)
#-------LINEAR MODEL------#
model1 <- lm(Value~t,data = train)
model1
summary(model1)
#Plotting the Linear model1
plot(model1)
#Predicting the Linear model by hiting on 'predict1'
predict1 <- predict(model1,test)
length(predict1)
length(test$Value)
error1 <- test$Value-predict1
rmse1 <- sqrt(sum(error1^2)/nrow(test)) 
rmse1 #276059630
m1 <- MAPE(predict1,test$Value)*100
m1 #80.46875

#-----EXPONENTIAL MODEL-----#
model2 <- lm(log_value~ t,data = train)
summary(model2)
#Plotting the Exp model2
plot(model2)
predict0 <- predict(model2,test)
predict2<- 2.718^predict0
# Caculating the Error for Model2
error2 <- test$Value-predict2
rmse2 <- sqrt(sum(error2^2)/nrow(test)) 
rmse2 #134967867
m2 <- MAPE(predict2,test$Value)*100
m2 #72.39539

#-----QUADRATIC MODEL-----#
model3 <- lm(Value ~ t+tsquare, data= train)
summary(model3) 
#Plotting the Quad model3
plot(model3)
predict3 <- predict(model3, test)
error3 <- test$Value-predict3
rmse3 <- sqrt(sum(error3^2)/nrow(test)) 
rmse3 #1150360381
m3 <- MAPE(predict3,test$Value)*100
m3 #93.79023

#-----ADDITIVE SEASONALITY-----#
model4 <- lm(Value~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model4)   
predict4 <- predict(model4, test)
error4 <- test$Value-predict4
rmse4 <- sqrt(sum(error4^2)/nrow(test)) 
rmse4 #41297807
m4 <- MAPE(predict4,test$Value)*100
m4 #45.04646

############# MULTIPLICATIVE SEASONALITY ############
model5 <- lm(log_value ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model5)   
#Plotting the model
plot(model5)
predict5 <- (predict(model5, test))
predict05 <- 2.718^predict5
error5 <- test$Value-predict05
rmse5 <- sqrt(sum(error5^2)/nrow(test)) 
rmse5 #40968909
m5 <- MAPE(predict05,test$Value)*100
m5 #45.03316

############# ADDITIVE WITH QUADRATIC ############
model6 <- lm(Value ~ t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model6)  
predict6 <- predict(model6, test)
predict6
error6 <- test$Value-predict6
rmse6 <- sqrt(sum(error6^2)/nrow(test)) 
rmse6 #26446113722      
m6 <- MAPE(predict6,test$Value)*100
m6 #99.84097
#Creating the RMSE table
rmsetable <- as.data.frame(rbind(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6))
View(rmsetable)
#Since Model5 has a least RMSE value but has very high forecasting RMSE value 
#for getting the correct statement going with DATA-DRIVEN APPROACH


#----DATA DRIVEN----#
train1 <- ts(data = forecasting1[1:12],frequency = 12)
test1 <- ts(data=forecasting1[13:24],frequency = 12)
plot(forecasting1)
#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter

#----SIMPLE EXPONENTIAL SMOTHING----#
simpelexpo <- HoltWinters(train1,alpha = .2,beta = F,gamma = F)
simpelexpo
simplepredict <- as.data.frame(predict(simpelexpo,n.ahead = 12))
simplepredict$fit
plot(forecast(simpelexpo,h=12))
#While driven to the graph no seasonality is found in the data
a <- MAPE(test1,simplepredict$fit)
a #4.173229

##alpha=.02 and beta =.01 and gamma = f 
##assume that  there is level and trend is present 
#----DOUBLE EXPONENTIAL SMOTHING----# 
double <- HoltWinters(train1,alpha = .2,beta=.1,gamma = F)
double
double$fitted
doublepredict <- as.data.frame(predict(double,n.ahead = 12))
doublepredict
plot(forecast(double,h=12))
# Still no seasonality is found in the data
c <- MAPE(test1,doublepredict$fit)*100
c #959.8621

#----WITHOUT OPTIMUM VALUE---# 
first <- HoltWinters(train1,beta = F,gamma = F)
firstprediict <- as.data.frame(predict(first,n.ahead = 12))
firstprediict
plot(forecast(first,h=12))
# Still no pattern tobe found
g <- MAPE(test1,firstprediict$fit)*100
g #425.9538
second <- HoltWinters(train1,gamma = F)
secondpredict=as.data.frame(predict(second,n.ahead = 12))
secondpredict
plot(forecast(second,h=12))
# Still no pattern tobe found
i <- MAPE(test1,secondpredict$fit)*100
i #738.285

finalresult <- data.frame(rbind(a,c,g,i,m1,m2,m3,m4,m5,m6))
View(finalresult)
#Inferences can be drawn from the data that 'a' in Data Driven Approach can be used for forecasting. 
