# R Project 
#Fernando Delgado, Nithesh Ramanna, Aazad Ghoslya

#===============================================================================
# Libraries
#===============================================================================

library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr) 
library(readxl)
library(tidyverse)
library(rstudioapi)               

#===============================================================================
# Functions Used
#===============================================================================

#function retrieved from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#===============================================================================
# Read in Data
#===============================================================================

#Load data
load("~/Group Assignment - Team 13/data/raw/DataGroupAssignment.Rdata")

#Write to csv
write.csv(UserDailyAggregation,"~/Group Assignment - Team 13/data/raw/uda.csv", row.names = FALSE)
write.csv(Demographics,"~/Group Assignment - Team 13/data/raw/demographics.csv", row.names = FALSE)
write.csv(PokerChipConversions,"~/Group Assignment - Team 13/data/raw/poker.csv", row.names = FALSE)

#read csv
UserDailyAggregation <- fread("~/Group Assignment - Team 13/data/raw/uda.csv")
PokerChipConversions <- fread("~/Group Assignment - Team 13/data/raw/poker.csv")
Demographics <- fread("~/Group Assignment - Team 13/data/raw/demographics.csv")

#Reading Appendices
Product <- read_excel("~/Group Assignment - Team 13/data/raw/Appendices Group Assignment.xlsx", sheet = "Appendix 1")
Country <- read_excel("~/Group Assignment - Team 13/data/raw/Appendices Group Assignment.xlsx", sheet = "Appendix 2")
Language <- read_excel("~/Group Assignment - Team 13/data/raw/Appendices Group Assignment.xlsx", sheet = "Appendix 3")
Application <- read_excel("~/Group Assignment - Team 13/data/raw/Appendices Group Assignment.xlsx", sheet = "Appendix 4")


#===============================================================================
#===============================================================================
#                                 DEMOGRAPHICS
#===============================================================================
#===============================================================================



#===============================================================================
# Data cleaning - Demographics
#===============================================================================

length(unique(Demographics$UserID)) # 42649 Unique IDs

#Converting the columns date types
Demographics$RegDate <- ymd(Demographics$RegDate)
Demographics$FirstPay <- ymd(Demographics$FirstPay)
Demographics$FirstAct <- ymd(Demographics$FirstAct)
Demographics$FirstSp <- ymd(Demographics$FirstSp)
Demographics$FirstCa <- ymd(Demographics$FirstCa)
Demographics$FirstGa <- ymd(Demographics$FirstGa)
Demographics$FirstPo <- ymd(Demographics$FirstPo)

#Merge them with Appendices 
Demographics <- merge(Demographics, Country, by="Country")
Demographics <- merge(Demographics,Application,by="ApplicationID")
Demographics <- merge(Demographics, Language, by = 'Language')
Demographics <- Demographics %>% select(-c('ApplicationID','Country', 'Language'))

#Fix Column Names, values, and Missing values
names(Demographics)[10]='Country'
names(Demographics)[11]='Application'
names(Demographics)[12]='Language'
Demographics$Gender <- ifelse(Demographics$Gender == 0, 'F', 'M')
Demographics[is.na(Demographics)]

#No need to Filter Accounts by dates
max(Demographics$RegDate) #Feb 27
min(Demographics$RegDate) #Feb 01

#Filter NA Active Date
Demographics <- Demographics[!is.na(Demographics$FirstAct),]

length(unique(Demographics$UserID)) #42647 Unique IDs

#===============================================================================
# Creating Variables - Demographics
#===============================================================================

#Find the days between Registration and First Pay
Demographics <- Demographics %>% mutate('DaysBeforePay' = Demographics$FirstPay - Demographics$RegDate)



#===============================================================================
#===============================================================================
#                           USER DAILY AGGREGATION
#===============================================================================
#===============================================================================



#===============================================================================
# Data Cleaning - UDA
#===============================================================================

#Converting the columns date types
UserDailyAggregation$Date = ymd(UserDailyAggregation$Date)

#Adding Product ID
UserDailyAggregation <- merge(UserDailyAggregation,Product,by="ProductID")

#Merge with First Pay
UserDailyAggregation <- merge(x = UserDailyAggregation, y = Demographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)
UserDailyAggregation$FirstPay <- ymd(UserDailyAggregation$FirstPay)
UserDailyAggregation$datedummy <- ifelse(UserDailyAggregation$Date < UserDailyAggregation$FirstPay, 1, 0)

#UDA has more Unique IDs than Demographics
sum(UserDailyAggregation$datedummy)
length(unique(UserDailyAggregation$UserID))#43851 Unique UDA User IDs
length(unique(Demographics$UserID))#42647 unique Demographics User IDs

#Filter NAs in FirstPayin
UserDailyAggregation <- UserDailyAggregation[!is.na(UserDailyAggregation$FirstPay),]

#Filter dates before First Pay
sum(UserDailyAggregation$datedummy) #Filter 2109 rows
UserDailyAggregation <- subset(UserDailyAggregation, datedummy == 0)
UserDailyAggregation <- subset(UserDailyAggregation, select = -c(datedummy, FirstPay))

#No need to filter by Dates
max(UserDailyAggregation$Date) #Sept 30
min(UserDailyAggregation$Date) #Feb 01

length(unique(UserDailyAggregation$UserID))#42203 Unique IDs

#===============================================================================
# Creating Variables - UDA
#===============================================================================


#Function for transposing by UserID by ProductID
transpose <- function(x) {

  #Subset by product
  subsetz <- UserDailyAggregation[UserDailyAggregation$ProductID == x, ]
  
  #Sum calculation
  subsetz_sums <- aggregate(list(subsetz$Stakes, subsetz$Winnings, subsetz$Bets), 
                        by = list(subsetz$UserID), 
                         FUN = sum, na.rm = TRUE)
  
  #Change Column Name
  concatstakes <- paste("TotalStakes_Product", x, sep = "" )
  concatwinnings <- paste("TotalWinnings_Product", x, sep = "" )
  concatbets <- paste("TotalBets_Product", x, sep = "" )
  concatltv <- paste("LifetimeValue_Product", x, sep = "")
  
  names(subsetz_sums)[1]= "UserID"
  names(subsetz_sums)[2]= concatstakes
  names(subsetz_sums)[3]= concatwinnings
  names(subsetz_sums)[4]= concatbets
  
  #LifetimeValue by game
  subsetz_sums[,concatltv] <- subsetz_sums[concatstakes] - subsetz_sums[concatwinnings]
  
  
  #Mean Calculation
  subsetz_mean <- aggregate(list(subsetz$Stakes, subsetz$Winnings, subsetz$Bets), 
                            by = list(subsetz$UserID), 
                            FUN = mean, na.rm = TRUE)
  
  #Change Column Name
  concatstakes2 <- paste("AverageDailyStake_Product", x, sep = "" )
  concatwinnings2 <- paste("AverageDailyWinning_Product", x, sep = "" )
  concatbets2 <- paste("AverageDailyBets_Product", x, sep = "" )
  
  names(subsetz_mean)[1]= "UserID"
  names(subsetz_mean)[2]= concatstakes2
  names(subsetz_mean)[3]= concatwinnings2
  names(subsetz_mean)[4]= concatbets2
  
  #Merge
  subsetz_merged <- merge(x = subsetz_sums, y = subsetz_mean, by = "UserID", all.x=TRUE)
  return(subsetz_merged)
  
}

#Transpose for all products
product1_merged <- transpose(1)
product2_merged <- transpose(2)
product4_merged <- transpose(4)
product5_merged <- transpose(5)
product6_merged <- transpose(6)
product7_merged <- transpose(7)
product8_merged <- transpose(8)

#Calculation of Variables regardless of the game

#Preferred Day of Play
UserDailyAggregation$dayofplay <- weekdays(UserDailyAggregation$Date)
othergames_dayofplay <- aggregate(dayofplay ~ UserID, UserDailyAggregation, Mode)

#Month with most transactions
UserDailyAggregation$monthofplay <- month(UserDailyAggregation$Date)
othergames_monthofplay <- aggregate(monthofplay ~ UserID, UserDailyAggregation, Mode)
othergames_monthofplay$monthofplay <- month.abb[othergames_monthofplay$monthofplay]

#Date of Last Transaction Other Games
lastday_uda <- aggregate(Date ~ UserID, UserDailyAggregation, FUN=max) 
names(lastday_uda)[2]='last_othergames_date'

#Date of First Transaction Other Games
firstday_uda <- aggregate(Date ~ UserID, UserDailyAggregation, FUN=min) 
names(firstday_uda)[2]='first_othergames_date'

#total stakes (all games)
totalstakes<- aggregate(Stakes ~ UserID, data = UserDailyAggregation, FUN = sum, na.rm = TRUE)
names(totalstakes)[2]='total_stakes_othergames'

#total winnings (all games)
totalwinnings<- aggregate(Winnings ~ UserID, data = UserDailyAggregation, FUN = sum, na.rm = TRUE)
names(totalwinnings)[2]='total_winnings_othergames'

#total bets (all games)
totalbets<- aggregate(Bets ~ UserID, data = UserDailyAggregation, FUN = sum, na.rm = TRUE)
names(totalbets)[2]='total_bets_othergames'

#===============================================================================
# Merged UDA Table for Data Mart
#===============================================================================

#Filter Unique IDs
uda_uniqueID <- data.frame(unique(UserDailyAggregation$UserID))
names(uda_uniqueID)[1]='UserID'
length(unique(uda_uniqueID$UserID)) #42203 unique IDS

#Left Join products
uda <- merge(x = uda_uniqueID, y = product1_merged, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = product2_merged, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = product4_merged, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = product5_merged, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = product6_merged, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = product7_merged, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = product8_merged, by = "UserID", all.x=TRUE)

#Join general variales
uda <- merge(x = uda, y = othergames_dayofplay, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = othergames_monthofplay, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = lastday_uda, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = firstday_uda, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = totalstakes, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = totalwinnings, by = "UserID", all.x=TRUE)
uda <- merge(x = uda, y = totalbets, by = "UserID", all.x=TRUE)

#validation: 42203
length(unique(uda$UserID))



#===============================================================================
#===============================================================================
#                             PokerChipsConversions
#===============================================================================
#===============================================================================



#===============================================================================
# Data Cleaning - PokerChipsConversions
#===============================================================================

#remove scientific notation
options(scipen=999)

#2387 Unique Users and tere with no NA values
length(unique(PokerChipConversions$UserID))
which(is.na(PokerChipConversions))

#Format Transaction Amount
PokerChipConversions$TransAmount <- round(PokerChipConversions$TransAmount, digits = 4)

#Format Transaction Type
PokerChipConversions$TransType <- ifelse(PokerChipConversions$TransType == 124, 'Buy', 'Sell')
length(unique(PokerChipConversions$TransType))

#Format Transaction Date and Time
PokerChipConversions<- separate(PokerChipConversions, col = TransDateTime, into = c("trans_date","trans_time"), sep = " ")
PokerChipConversions$trans_date <- ymd(PokerChipConversions$trans_date)

PokerChipConversions$trans_hour <- format(as.POSIXct(PokerChipConversions$trans_time, format="%H:%M:%S"),"%H")
PokerChipConversions$trans_hour <- as.integer(PokerChipConversions$trans_hour)

#Filter transactions that happened before First Pay in
PokerChipConversions <- merge(x = PokerChipConversions, y = Demographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)
PokerChipConversions$FirstPay <- ymd(PokerChipConversions$FirstPay)
PokerChipConversions$datedummy <- ifelse(PokerChipConversions$trans_date < PokerChipConversions$FirstPay, 1, 0)

sum(PokerChipConversions$datedummy)#60 filtered rows

PokerChipConversions <- subset(PokerChipConversions, datedummy == 0)
PokerChipConversions <- subset(PokerChipConversions, select = -c(datedummy, FirstPay, trans_time))

#Filter transactions after today
today <- ymd(20050930)
PokerChipConversions$datedummy <- ifelse(PokerChipConversions$trans_date > today, 1, 0)

sum(PokerChipConversions$datedummy)#2427 filtered rows

PokerChipConversions <- subset(PokerChipConversions, datedummy == 0)
PokerChipConversions <- subset(PokerChipConversions, select = -datedummy)

#We end up with a table of 2368 unique IDs
length(unique(PokerChipConversions$UserID))

#===============================================================================
# Creating Variables - PokerChipConversions
#===============================================================================

#Most Frequent Time of Play
PokerChipConversions$TimeofPlay <- ifelse(PokerChipConversions$trans_hour > 18, 'Night', 
                                          ifelse(PokerChipConversions$trans_hour > 12, 'Afternoon',
                                                 ifelse(PokerChipConversions$trans_hour > 6, 'Early Bird', 'Late Night')))

mode_timofplay <- aggregate(TimeofPlay ~ UserID, PokerChipConversions, Mode) 

#Preferred Day of Transaction
PokerChipConversions$dayofplay <- weekdays(PokerChipConversions$trans_date)
dayofplay <- aggregate(dayofplay ~ UserID, PokerChipConversions, Mode)

#Month with most Transactions
PokerChipConversions$month <- month(PokerChipConversions$trans_date)
monthofplay <- aggregate(month ~ UserID, PokerChipConversions, Mode)
monthofplay$month <- month.abb[monthofplay$month]

#Aggregation of Buy/Sell
sums <- aggregate(TransAmount ~ UserID + TransType, data = PokerChipConversions, FUN = sum, na.rm = TRUE)
sums_transpose <- sums %>% spread(key = TransType, value = TransAmount)
sums_transpose[is.na(sums_transpose)] <- 0

#Lifetime Value per Customer (How much profits they are generating for the Casino)
sums_transpose$poker_ltv <- sums_transpose$Buy - sums_transpose$Sell
names(sums_transpose)[2]='totalpoker_buy'
names(sums_transpose)[3]='totalpoker_sell'

#Calculation of Poker Transaction Frequency
frequency <- aggregate(PokerChipConversions$TransAmount, by=list(PokerChipConversions$UserID), FUN=length)
names(frequency)[1]='UserID'
names(frequency)[2] <- "poker_frequency"

#Calculation of Poker Recency (Time since last game played)
today <- ymd(20050930)
lastday_pokertrans <- aggregate(trans_date ~ UserID, PokerChipConversions, FUN=max)
names(lastday_pokertrans)[2]='last_poker_date'
lastday_pokertrans$poker_recency <- today - lastday_pokertrans$last_poker_date

#Date of First Transaction Poker
firstday_pokertrans <- aggregate(trans_date ~ UserID, PokerChipConversions, FUN=min) 
names(firstday_pokertrans)[2]='first_pokertrans_date'

#===============================================================================
# Aggregated Poker Table for Data Mart
#===============================================================================

#Filter Unique IDs
uniqueID <- data.frame(unique(PokerChipConversions$UserID))
names(uniqueID)[1]='UserID'
length(unique(uniqueID$UserID)) #2368 unique IDS

#Left Join
poker <- merge(x = uniqueID, y = sums_transpose, by = "UserID", all.x=TRUE)
poker <- merge(x = poker, y = frequency, by = "UserID", all.x=TRUE)
poker <- merge(x = poker, y = firstday_pokertrans, by = "UserID", all.x=TRUE)
poker <- merge(x = poker, y = lastday_pokertrans, by = "UserID", all.x=TRUE)
poker <- merge(x = poker, y = mode_timofplay, by = "UserID", all.x=TRUE)
poker <- merge(x = poker, y = dayofplay, by = "UserID", all.x=TRUE)
poker <- merge(x = poker, y = monthofplay, by = "UserID", all.x=TRUE)

#format column names
poker$poker_ltv <- round(poker$poker_ltv, digits = 4)
names(poker)[9]='poker_Pref_timeofplay'
names(poker)[10]='poker_Pref_DayofPlay'
names(poker)[11]='poker_Pref_MonthofPlay'
poker$poker_recency <- as(poker$poker_recency, "numeric")

#Merge Validation
length(unique(poker$UserID)) #2368 unique IDS



#===============================================================================
#===============================================================================
#                             Merging Datamart
#===============================================================================
#===============================================================================


#merge datamart
datamart <- merge(x = Demographics, y = uda, by = "UserID", all.x=TRUE)
datamart <- merge(x = datamart, y = poker, by = "UserID", all.x=TRUE)

#42647 Unique Ids and 78 columns
str(datamart)

#calculate overall max date and recency
datamart$totalmaxdate <- pmax(datamart$last_poker_date, datamart$last_othergames_date, na.rm = TRUE)
datamart$totalrecency <- today - datamart$totalmaxdate

#sum of frequency and number of bets (total frequency) 
datamart <- mutate_at(datamart, c("poker_frequency", "total_bets_othergames"), ~replace(., is.na(.), 0))
datamart$totalfrequency <- datamart$poker_frequency + datamart$total_bets_othergames

#sum of total stakes and buy (total revenue)
datamart <- mutate_at(datamart, c("total_stakes_othergames", "totalpoker_buy"), ~replace(., is.na(.), 0))
datamart$totalrevenue <- datamart$total_stakes_othergames + datamart$totalpoker_buy

#sum of total winnings and total sell (total cost)
datamart <- mutate_at(datamart, c("total_winnings_othergames", "totalpoker_sell"), ~replace(., is.na(.), 0))
datamart$totalcost <- datamart$total_winnings_othergames + datamart$totalpoker_sell

#Total Lifetime Value (revenue - cost)
datamart$lifetimevalue <- datamart$totalrevenue - datamart$totalcost

#Profit margin (Lifetimevalue/total revenue)
datamart$profitmargin <- datamart$lifetimevalue / datamart$totalrevenue

#===============================================================================
# RFM Model
#===============================================================================

#The following model splits Recency Frequency and Monetary Value into quantiles to assign levels.
#Then, we sum the 3 levels into a general RFM Score and assign customer segments based on the total score

#Divide values into quantiles
datamart <- datamart %>% mutate(r_level = ntile(-totalrecency, 4)) #levels are inverse because the lower the recency the better
datamart <- datamart %>% mutate(f_level = ntile(totalfrequency, 4))
datamart <- datamart %>% mutate(m_level = ntile(lifetimevalue, 4))

#General Score
datamart$rfm_level <- datamart$r_level + datamart$f_level + datamart$m_level

#Labels by Level (max score is 12, min score is 3)
datamart$customer_level <- ifelse(datamart$rfm_level > 10 , 'Platinum', 
                                      ifelse(datamart$rfm_level > 8 , 'Gold', 
                                             ifelse(datamart$rfm_level > 6 , 'Silver','Bronze')))

#Visualization of Segment Distribution: 144 Bronze, 779 Silver, 841 Gold, and 604 Platinum
pie(sort(table(datamart$customer_level)))
table(datamart$customer_level)

#Filled NA in gender column with "M" as Male count is high
datamart$Gender[is.na(datamart$Gender)] <- "M"

str(datamart)

write.csv(datamart,"~/Group Assignment - Team 13/data/processed/datamart.csv", row.names = FALSE)


