setwd("C:\\Users\\HP\\Desktop\\Project")
#Installing the libraries
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("moments")
#install.packages("graphics")

#Accessing libraries:::
library(readr)
library(ggplot2)
library(dplyr)
library(moments)
library(graphics)

#Importing the files
Cheque <- read.csv(file.choose())
ChequeD <- Cheque
View(ChequeD)  

#Checking the missing values chequedata::
sum(is.na(ChequeD)) #No missimg value needs to be found
str(ChequeD) 
summary(ChequeD)

ChequeD <- ChequeD[c(1,4)] #removing unwanted data

#Normality test
attach(ChequeD)
shapiro.test(Debit.Amount)
#value of p = 1.455e-15<0.05 follows normal distribution
#sd(Debit.Amount)
#z.test(Debit.Amount, mu = 96383.89, 
#       sigma.y = 131204.9, conf.level = 0.95, correct=TRUE)

#Density plot
plot(density(Debit.Amount))

#Using qqplot
qqnorm(Debit.Amount);qqline(Debit.Amount, col=3)


#------------EDA------------------#
#Business Moment's
#Measure of Central Tendency
mean(ChequeD$Debit.Amount) #96383.89

#mode
getmode <- function(y){
  uniquv <- unique(y)
  uniquv[which.max(tabulate(match(y,uniquv)))]
}
getmode(ChequeD$Debit.Amount)

#Measures of Dispersion
var(ChequeD$Debit.Amount) #17214728389
sd(ChequeD$Debit.Amount) #131204.9
range(ChequeD$Debit.Amount)#7376 691861
rangevalue <- function(y)
{
  max(y)-min(y)
  }
rangevalue(ChequeD$Debit.Amount) #684485

#3rd Business Moment
skewness(ChequeD$Debit.Amount) #2.802649
#4th Bussiness Moments
kurtosis(ChequeD$Debit.Amount) #10.7612

windows()
ggplot(data = ChequeD,aes(x = Debit.Amount, fill = Dealer.ID))+geom_histogram(bins = 30)


hist(ChequeD$Debit.Amount, xlab = "Debit Amount", main = "Distribution of Debit Amount", col = "aquamarine3")
#from the Boxplot infernces can be drawn that the value of Debit is decreasing

#Checking the outlier's
dim(ChequeD)
sum(is.null(ChequeD))
quantile(ChequeD$Debit.Amount)
Cheqd <- subset(ChequeD,ChequeD$Debit.Amount<25366.25)
# After removing the outlier's
boxplot(Cheqd$Debit.Amount)
dim(Cheqd)


