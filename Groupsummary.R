#Importing the libraries
library(readr)
library(ggplot2)
library(dplyr)
library(moments)
library(graphics)

Grpsumm <- read.csv(file.choose())
Grpsumm <- Grpsumm
names(Grpsumm)[1] <- "Dealer.ID"
names(Grpsumm)[2] <-"Opening Balance"
names(Grpsumm)[3] <-"Debit"
names(Grpsumm)[4] <-"Credit"
names(Grpsumm)[5] <-"Closing Balance"
head(Grpsumm)

mydata <- Grpsumm[c(2:1024),]
rownames(mydata) <- NULL
mydata <- fix(mydata)
str(mydata)
attach(mydata)

#Cleansing the Data:
#Dealer.ID <- replace[Dealer.ID, Dealer.ID == 'Area Manager( 02105)', "Area Manager( 02105)"]
#mydata$Dealer.ID <- replace[Dealer.ID==""|Dealer.ID=="Area Manager( 02105)"] 

#levels(mydata$Dealer.ID) <- c(levels(mydata$Dealer.ID), "SO (2105)")
#mydata$Dealer.ID[mydata$Dealer.ID == 'SO (2105)	'] <- 'SO (2105)'  

levels(mydata$Dealer.ID)[5] <- 'AREA MANAGER ( 02105 )'
head(mydata)

sum(is.na(mydata)) #no NA tobe found in dataset
class(mydata)







 
