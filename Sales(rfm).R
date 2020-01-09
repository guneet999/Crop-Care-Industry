setwd("C://Users//HP//Desktop//Project")
#Importing the libraries
#install.packages("readr")
library(readr)

#Uploading the Sales Dataset
Sales <- read.csv("C://Users//HP//Desktop//Project//Sales.csv")
View(Sales)
#Ignoring the unwanted data
Sales <- Sales[,-c(5,6,7)]

#formatting the dates
Sales$Date <- as.Date(Sales$Date,"%m/%d/%Y")

#RFM
#install.packages("dplyr")
library(dplyr)
library(knitr)
#how recent the customer purchased the product
Recency <- Sales %>% 
  group_by(Dealer.ID) %>% 
  summarise(Recency=as.numeric(as.Date("2018-04-01")-max(Date)))
View(Recency)        
kable(head(Recency))

#calculating the frequency of dealer:
#install.packages("plyr")
library(plyr)
freq = ddply(Sales,. (Dealer.ID, Date), summarize, freq = sum(Value), freq = length(Dealer.ID))
freq = ddply(freq,. (Dealer.ID), summarize, freq = length(Dealer.ID))
View(freq)

#Calcualting the monetary of the data
monet <- ddply(Sales,. (Dealer.ID), summarize, freq_value = sum(Value))
df_monet <- merge(freq,monet, by="Dealer.ID")
df_monet$monetary=df_monet$freq_value/df_monet$freq
#Removing the monetary from the data 
df_monet <- df_monet[,-4]
View(df_monet)

# Combing the Recency, frequency, monetarywith dealer.ID
Sales_rfm <- merge(Recency, df_monet, by="Dealer.ID")

# Rename of column
library(data.table)
colnames(Sales_rfm)
setnames(Sales_rfm, old=c("freq","freq_value"), new=c("frequency", "monetory"))
View(Sales_rfm) 

#Saving the RFM file of Sales data:
write.csv(Sales_rfm, file = "Sales_rfm.csv")

#Analyzing the data
hist(Sales_rfm$Recency)
hist(Sales_rfm$freq)
hist(Sales_rfm$monetory)

##############################################################################

library(readr)
sales_RFM <- read.csv("C:/Users/HP/Desktop/Project/Sales_rfm.csv")
sales_RFM <- sales_RFM[-1]
View(sales_RFM)


normalized_data <- scale(sales_RFM[,2:4])
View(normalized_data)

final <- data.frame(sales_RFM, normalized_data)
View(final)

## To calculate CLV for each cluster, weighed RFM method is used. 
## WF is 0.637, WM is 0.258 and finally WR is 0.105. 
## clv = (NR * WR) + (NF * WF) + (NM * WM)
colnames(final)

# clv = (recency.1 * 0.105) + (frequency.1 * 0.637) + (monetory.1 * 0.258)
final[,6]*0.105 + final[,7]*0.637 + final[,8]*0.258
final$clv <- final[,5]*0.105 + final[,6]*0.637 + final[,7]*0.258
View(final)
kable(head(final))
write.csv(final, file = "Sales_clv.csv")
##############################################################################

#RFM Score

# 70   very good  5
# 140  good       4
# 220  average    3
# 290  low        2
# 290+ bad        1

sales_RFM$Recency <- ifelse(sales_RFM$Recency <= 70,'5',
                       ifelse(70<sales_RFM$Recency &sales_RFM$Recency<=140,'4',
                              ifelse(140<sales_RFM$Recency & sales_RFM$Recency<=220,'3',ifelse(220<sales_RFM$Recency & sales_RFM$Recency<=290,"2","1"))))
max(sales_RFM$Recency)

#  35+     very good
#  25+ 35- good
#  15+ 25- average
#  5+ 15-  low
#  below 5 bad


sales_RFM$frequency <- ifelse(sales_RFM$frequency >=35,'5',
                       ifelse(35 > sales_RFM$frequency &sales_RFM$frequency >25,'4',
                              ifelse(25 > sales_RFM$frequency & sales_RFM$frequency >15,'3',
                                     ifelse(15 > sales_RFM$frequency & sales_RFM$frequency >10,"2","1"))))

max(sales_RFM$frequency)

## ## #i will take 1300000+ as very good
## 8lkh+ 1300000- as good
## 5+ 8- as average
## 2+ 5-low
#  below 2 bad

sales_RFM$monetory <- ifelse(sales_RFM$monetory >=1300000,'5',
                       ifelse(1300000 > sales_RFM$monetory &sales_RFM$monetory >800000,'4',
                              ifelse(800000 > sales_RFM$monetory & sales_RFM$monetory >500000,'3',
                                     ifelse(500000 > sales_RFM$monetory & sales_RFM$monetory >200000,"2","1"))))


sales_RFM$RFM<- with(sales_RFM, paste0(sales_RFM$Recency, sales_RFM$frequency, sales_RFM$monetory))


max(sales_RFM$RFM)
min(sales_RFM$RFM)

#RFM score btween 450  to 551 "1"  Very goood
#RFM score between 350 to 449 "2"  good
#RFM score between 250 to 349 "3"  Average
#RFM Score between 150 to 249 "4"  low
#if rfm less than 150 the     "5"  Bad


sales_RFM$Creditscore <- ifelse(sales_RFM$RFM>=450,'5',
                                 ifelse(450 > sales_RFM$RFM &sales_RFM$RFM >350,'4',
                                        ifelse(350 > sales_RFM$RFM & sales_RFM$RFM >250 ,'3',
                                               ifelse(250 > sales_RFM$RFM & sales_RFM$RFM >150,"2","1"))))


#####################################################

#Calculating the credit Score
finaldata=cbind(sales_RFM,final)
finaldata=finaldata[c(1:6)]
kable(head(finaldata))
View(finaldata)

#####################################################