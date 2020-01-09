#Importing the libraries
library(readr)
library(ggplot2)
library(dplyr)
library(moments)
library(graphics)

#Uploading the file
Receipt <- read.csv(file.choose())
Data <- Receipt
Data <- Receipt[c("Date","Dealer.ID", "Credit.Amount")]
str(Data)
summary(Data)

#Missing data
sum(is.na(Data)) #no NA val. tobe found

#formatting the dates
Data$Date <- as.Date(Data$Date,"%m/%d/%Y")
str(Data$Date)
order(Data$Date)
sorted <- Data[order(Data$Date),]
View(sorted)
diff(sorted$Date) #time differnce 

attach(sorted)

#Normality test
shapiro.test(Credit.Amount)
#value of p = 2.2e-16<0.5 follows normal distribution

#Density plot
plot(density(Credit.Amount))
#Using qqplot
qqnorm(Credit.Amount);qqline(Credit.Amount, col=2)

is.null(sorted) 
#Inferences can be drawn above datasets returning FALSE means there is no null val.
#Data Analysis of bussiness moments

#Measue of central tendency
mean(Credit.Amount) #121862.3 

getmode <- function(x)
{
  alpha <- alpha(x)
  alpha[which.max(tabulate(match(x,alpha())))]
}
getmode(sorted$Credit.Amount)

#Measure iof dispersion
var(Credit.Amount)
sd(Credit.Amount)
range(Credit.Amount) # 1 7083069

rangevalue <- function(p)
{
  max(p)-min(p)
}
rangevalue(Credit.Amount) #7083068

#3rd and 4th moment
skewness(Credit.Amount) #9.448097
kurtosis(Credit.Amount) #107.7393

#install.packages("lubridate")
#library(lubridate)

windows()
ggplot(sorted)+aes(x=Date,y=Credit.Amount)+geom_point(col = "deeppink3", alpha = 0.1)

#Checking the Outlier's in the dataset's
#1st Way
dim(sorted)
boxplot(Credit.Amount)
quantile(Credit.Amount)
#Out <- c(sorted$Credit.Amount[which(sorted$Credit.Amount<24876)])
Out <- subset(sorted,sorted$Credit.Amount<24876)
boxplot(Out)
dim(Out) #dim of outlier's 791x3

#2nd Way
# Dimension of 'sorted' datsets
dim(sorted)
# Storing the outlier's in a vector
#outliers <- boxplot(sorted$Credit.Amount, plot = FALSE)$out
# Removing the outlier's
#sorted[which(Credit.Amount %in% outliers),]
#remove_out <- sorted[-which(sorted$Credit.Amount %in% outliers),]
#windows()
#boxplot(remove_out$Credit.Amount)
#dim(remove_out)




#Mean is been computed using res:::
#3rd Way
#res <- boxplot(sorted, outline = FALSE)

#Computing the mean withot the Outlier's
mean(sorted$Credit.Amount[!sorted$Credit.Amount %in% res$out]) # 53902.34
help("%in%")
