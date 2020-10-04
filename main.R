## Checkpoint 1: Data Understanding & Data Preparation

# Installing Libraries
require(lubridate)
library(forecast)
library(graphics)
library("dplyr")

# Reading dataset in Rstudio
global_data <- read.csv("Global Superstore.csv", stringsAsFactors = T, header = T, na.strings = c("","NA","n/a"), strip.white = T, fill = T)

############################################### EDA ######################################

###### Bar Chart: Records in each Region
library(plotly)

x <- names(table(global_data$Market))
y <- as.vector(table(global_data$Market))

fig <- plot_ly(global_data, x = ~x, y = ~y, type = 'bar',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)))
fig <- fig %>% layout(title = "Records in each Region",
                      xaxis = list(title = "Markets"),
                      yaxis = list(title = "Count of orders"))

fig


###### Bar Chart: Records per category
library(plotly)

x <- names(table(global_data$Category))
y <- as.vector(table(global_data$Category))

fig <- plot_ly(global_data, x = ~x, y = ~y, type = 'bar',
               marker = list(color = 'rgb(255, 0, 0, 0.6)',
                             line = list(color = 'rgb(20,20,20)',
                                         width = 1.5)))
fig <- fig %>% layout(title = "Records per category",
                      xaxis = list(title = "Category"),
                      yaxis = list(title = "Count of orders"))

fig

###### Bar Chart: Records per category
library(plotly)

x <- names(table(global_data$Category))
y <- as.vector(table(global_data$Category))

fig <- plot_ly(global_data, x = ~x, y = ~y, type = 'bar',
               marker = list(color = 'rgb(255, 0, 0, 0.6)',
                             line = list(color = 'rgb(20,20,20)',
                                         width = 1.5)))
fig <- fig %>% layout(title = "Records per category",
                      xaxis = list(title = "Category"),
                      yaxis = list(title = "Count of orders"))

fig


###### Bar Chart: Records per sub-category
library(plotly)

x <- names(table(global_data$Sub.Category))
y <- as.vector(table(global_data$Sub.Category))

fig <- plot_ly(global_data, x = ~x, y = ~y, type = 'bar',
               marker = list(color = 'rgb(255, 0, 0, 0.6)',
                             line = list(color = 'rgb(20,20,20)',
                                         width = 1.5)))
fig <- fig %>% layout(title = "Records per sub-category",
                      xaxis = list(title = "Sub-Category"),
                      yaxis = list(title = "Count of orders"))

fig

#############

summary(global_data$Segment)
str(global_data$Country)

sum(is.na(global_data$Sales))
sum(is.na(global_data$Profit))
sum(is.na(global_data$Quantity))

# Subsetting the dataset into 21 subsets wrt Market and Segment

AfricaConsumer <- data.frame(subset(global_data, global_data$Market == "Africa" & global_data$Segment == "Consumer"))
AfricaCorporate <- data.frame(subset(global_data, global_data$Market == "Africa" & global_data$Segment == "Corporate"))
AfricaHomeOffice <- data.frame(subset(global_data, global_data$Market == "Africa" & global_data$Segment == "Home Office"))

APACConsumer <- data.frame(subset(global_data, global_data$Market == "APAC" & global_data$Segment == "Consumer"))
APACCorporate <- data.frame(subset(global_data, global_data$Market == "APAC" & global_data$Segment == "Corporate"))
APACHomeOffice <- data.frame(subset(global_data, global_data$Market == "APAC" & global_data$Segment == "Home Office"))

CanadaConsumer <- data.frame(subset(global_data, global_data$Market == "Canada" & global_data$Segment == "Consumer"))
CanadaCorporate <- data.frame(subset(global_data, global_data$Market == "Canada" & global_data$Segment == "Corporate"))
CanadaHomeOffice <- data.frame(subset(global_data, global_data$Market == "Canada" & global_data$Segment == "Home Office"))

EMEAConsumer <- data.frame(subset(global_data, global_data$Market == "EMEA" & global_data$Segment == "Consumer"))
EMEACorporate <- data.frame(subset(global_data, global_data$Market == "EMEA" & global_data$Segment == "Corporate"))
EMEAHomeOffice <- data.frame(subset(global_data, global_data$Market == "EMEA" & global_data$Segment == "Home Office"))

EUConsumer <- data.frame(subset(global_data, global_data$Market == "EU" & global_data$Segment == "Consumer"))
EUCorporate <- data.frame(subset(global_data, global_data$Market == "EU" & global_data$Segment == "Corporate"))
EUHomeOffice <- data.frame(subset(global_data, global_data$Market == "EU" & global_data$Segment == "Home Office"))

LATAMConsumer <- data.frame(subset(global_data, global_data$Market == "LATAM" & global_data$Segment == "Consumer"))
LATAMCorporate <- data.frame(subset(global_data, global_data$Market == "LATAM" & global_data$Segment == "Corporate"))
LATAMHomeOffice <- data.frame(subset(global_data, global_data$Market == "LATAM" & global_data$Segment == "Home Office"))

USConsumer <- data.frame(subset(global_data, global_data$Market == "US" & global_data$Segment == "Consumer"))
USCorporate <- data.frame(subset(global_data, global_data$Market == "US" & global_data$Segment == "Corporate"))
USHomeOffice <- data.frame(subset(global_data, global_data$Market == "US" & global_data$Segment == "Home Office"))

## SALES

# Africa

AfricaCorporate_Sales <- aggregate(AfricaCorporate$Sales, by=list(paste(substring(AfricaCorporate$Order.Date,7,10), substring(AfricaCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaCorporate_Sales) <- c("month", "sales")
AfricaCorporate_Sales_TS <- ts(AfricaCorporate_Sales$sales)
plot(AfricaCorporate_Sales_TS)

AfricaConsumer_Sales <- aggregate(AfricaConsumer$Sales, by=list(paste(substring(AfricaConsumer$Order.Date,7,10), substring(AfricaConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaConsumer_Sales) <- c("month", "sales")
AfricaConsumer_Sales_TS <- ts(AfricaConsumer_Sales$sales)
plot(AfricaConsumer_Sales_TS)

AfricaHomeOffice_Sales <- aggregate(AfricaHomeOffice$Sales, by=list(paste(substring(AfricaHomeOffice$Order.Date,7,10), substring(AfricaHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaHomeOffice_Sales) <- c("month", "sales")
AfricaHomeOffice_Sales_TS <- ts(AfricaHomeOffice_Sales$sales)
plot(AfricaHomeOffice_Sales_TS)

# APAC

APACCorporate_Sales <- aggregate(APACCorporate$Sales, by=list(paste(substring(APACCorporate$Order.Date,7,10), substring(APACCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACCorporate_Sales) <- c("month", "sales")
APACCorporate_Sales_TS <- ts(APACCorporate_Sales$sales)
plot(APACCorporate_Sales_TS)

APACConsumer_Sales <- aggregate(APACConsumer$Sales, by=list(paste(substring(APACConsumer$Order.Date,7,10), substring(APACConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACConsumer_Sales) <- c("month", "sales")
APACConsumer_Sales_TS <- ts(APACConsumer_Sales$sales)
plot(APACConsumer_Sales_TS)

APACHomeOffice_Sales <- aggregate(APACHomeOffice$Sales, by=list(paste(substring(APACHomeOffice$Order.Date,7,10), substring(APACHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACHomeOffice_Sales) <- c("month", "sales")
APACHomeOffice_Sales_TS <- ts(APACHomeOffice_Sales$sales)
plot(APACHomeOffice_Sales_TS)

# Canada

CanadaCorporate_Sales <- aggregate(CanadaCorporate$Sales, by=list(paste(substring(CanadaCorporate$Order.Date,7,10), substring(CanadaCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaCorporate_Sales) <- c("month", "sales")
CanadaCorporate_Sales_TS <- ts(CanadaCorporate_Sales$sales)
plot(CanadaCorporate_Sales_TS)

CanadaConsumer_Sales <- aggregate(CanadaConsumer$Sales, by=list(paste(substring(CanadaConsumer$Order.Date,7,10), substring(CanadaConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaConsumer_Sales) <- c("month", "sales")
CanadaConsumer_Sales_TS <- ts(CanadaConsumer_Sales$sales)
plot(CanadaConsumer_Sales_TS)

CanadaHomeOffice_Sales <- aggregate(CanadaHomeOffice$Sales, by=list(paste(substring(CanadaHomeOffice$Order.Date,7,10), substring(CanadaHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaHomeOffice_Sales) <- c("month", "sales")
CanadaHomeOffice_Sales_TS <- ts(CanadaHomeOffice_Sales$sales)
plot(CanadaHomeOffice_Sales_TS)

# EMEA

EMEACorporate_Sales <- aggregate(EMEACorporate$Sales, by=list(paste(substring(EMEACorporate$Order.Date,7,10), substring(EMEACorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEACorporate_Sales) <- c("month", "sales")
EMEACorporate_Sales_TS <- ts(EMEACorporate_Sales$sales)
plot(EMEACorporate_Sales_TS)

EMEAConsumer_Sales <- aggregate(EMEAConsumer$Sales, by=list(paste(substring(EMEAConsumer$Order.Date,7,10), substring(EMEAConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEAConsumer_Sales) <- c("month", "sales")
EMEAConsumer_Sales_TS <- ts(EMEAConsumer_Sales$sales)
plot(EMEAConsumer_Sales_TS)

EMEAHomeOffice_Sales <- aggregate(EMEAHomeOffice$Sales, by=list(paste(substring(EMEAHomeOffice$Order.Date,7,10), substring(EMEAHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEAHomeOffice_Sales) <- c("month", "sales")
EMEAHomeOffice_Sales_TS <- ts(EMEAHomeOffice_Sales$sales)
plot(EMEAHomeOffice_Sales_TS)

# EU

EUCorporate_Sales <- aggregate(EUCorporate$Sales, by=list(paste(substring(EUCorporate$Order.Date,7,10), substring(EUCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUCorporate_Sales) <- c("month", "sales")
EUCorporate_Sales_TS <- ts(EUCorporate_Sales$sales)
plot(EUCorporate_Sales_TS)

EUConsumer_Sales <- aggregate(EUConsumer$Sales, by=list(paste(substring(EUConsumer$Order.Date,7,10), substring(EUConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUConsumer_Sales) <- c("month", "sales")
EUConsumer_Sales_TS <- ts(EUConsumer_Sales$sales)
plot(EUConsumer_Sales_TS)

EUHomeOffice_Sales <- aggregate(EUHomeOffice$Sales, by=list(paste(substring(EUHomeOffice$Order.Date,7,10), substring(EUHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUHomeOffice_Sales) <- c("month", "sales")
EUHomeOffice_Sales_TS <- ts(EUHomeOffice_Sales$sales)
plot(EUHomeOffice_Sales_TS)

# LATAM

LATAMCorporate_Sales <- aggregate(LATAMCorporate$Sales, by=list(paste(substring(LATAMCorporate$Order.Date,7,10), substring(LATAMCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMCorporate_Sales) <- c("month", "sales")
LATAMCorporate_Sales_TS <- ts(LATAMCorporate_Sales$sales)
plot(LATAMCorporate_Sales_TS)

LATAMConsumer_Sales <- aggregate(LATAMConsumer$Sales, by=list(paste(substring(LATAMConsumer$Order.Date,7,10), substring(LATAMConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMConsumer_Sales) <- c("month", "sales")
LATAMConsumer_Sales_TS <- ts(LATAMConsumer_Sales$sales)
plot(LATAMConsumer_Sales_TS)

LATAMHomeOffice_Sales <- aggregate(LATAMHomeOffice$Sales, by=list(paste(substring(LATAMHomeOffice$Order.Date,7,10), substring(LATAMHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMHomeOffice_Sales) <- c("month", "sales")
LATAMHomeOffice_Sales_TS <- ts(LATAMHomeOffice_Sales$sales)
plot(LATAMHomeOffice_Sales_TS)

# US

USCorporate_Sales <- aggregate(USCorporate$Sales, by=list(paste(substring(USCorporate$Order.Date,7,10), substring(USCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USCorporate_Sales) <- c("month", "sales")
USCorporate_Sales_TS <- ts(USCorporate_Sales$sales)
plot(USCorporate_Sales_TS)

USConsumer_Sales <- aggregate(USConsumer$Sales, by=list(paste(substring(USConsumer$Order.Date,7,10), substring(USConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USConsumer_Sales) <- c("month", "sales")
USConsumer_Sales_TS <- ts(USConsumer_Sales$sales)
plot(USConsumer_Sales_TS)

USHomeOffice_Sales <- aggregate(USHomeOffice$Sales, by=list(paste(substring(USHomeOffice$Order.Date,7,10), substring(USHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USHomeOffice_Sales) <- c("month", "sales")
USHomeOffice_Sales_TS <- ts(USHomeOffice_Sales$sales)
plot(USHomeOffice_Sales_TS)


# QUANTITY 

# Africa

AfricaCorporate_Quantity <- aggregate(AfricaCorporate$Quantity, by=list(paste(substring(AfricaCorporate$Order.Date,7,10), substring(AfricaCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaCorporate_Quantity) <- c("month", "Quantity")
AfricaCorporate_Quantity_TS <- ts(AfricaCorporate_Quantity$Quantity)
plot(AfricaCorporate_Quantity_TS)

AfricaConsumer_Quantity <- aggregate(AfricaConsumer$Quantity, by=list(paste(substring(AfricaConsumer$Order.Date,7,10), substring(AfricaConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaConsumer_Quantity) <- c("month", "Quantity")
AfricaConsumer_Quantity_TS <- ts(AfricaConsumer_Quantity$Quantity)
plot(AfricaConsumer_Quantity_TS)

AfricaHomeOffice_Quantity <- aggregate(AfricaHomeOffice$Quantity, by=list(paste(substring(AfricaHomeOffice$Order.Date,7,10), substring(AfricaHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaHomeOffice_Quantity) <- c("month", "Quantity")
AfricaHomeOffice_Quantity_TS <- ts(AfricaHomeOffice_Quantity$Quantity)
plot(AfricaHomeOffice_Quantity_TS)

# APAC

APACCorporate_Quantity <- aggregate(APACCorporate$Quantity, by=list(paste(substring(APACCorporate$Order.Date,7,10), substring(APACCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACCorporate_Quantity) <- c("month", "Quantity")
APACCorporate_Quantity_TS <- ts(APACCorporate_Quantity$Quantity)
plot(APACCorporate_Quantity_TS)

APACConsumer_Quantity <- aggregate(APACConsumer$Quantity, by=list(paste(substring(APACConsumer$Order.Date,7,10), substring(APACConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACConsumer_Quantity) <- c("month", "Quantity")
APACConsumer_Quantity_TS <- ts(APACConsumer_Quantity$Quantity)
plot(APACConsumer_Quantity_TS)

APACHomeOffice_Quantity <- aggregate(APACHomeOffice$Quantity, by=list(paste(substring(APACHomeOffice$Order.Date,7,10), substring(APACHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACHomeOffice_Quantity) <- c("month", "Quantity")
APACHomeOffice_Quantity_TS <- ts(APACHomeOffice_Quantity$Quantity)
plot(APACHomeOffice_Quantity_TS)

# Canada

CanadaCorporate_Quantity <- aggregate(CanadaCorporate$Quantity, by=list(paste(substring(CanadaCorporate$Order.Date,7,10), substring(CanadaCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaCorporate_Quantity) <- c("month", "Quantity")
CanadaCorporate_Quantity_TS <- ts(CanadaCorporate_Quantity$Quantity)
plot(CanadaCorporate_Quantity_TS)

CanadaConsumer_Quantity <- aggregate(CanadaConsumer$Quantity, by=list(paste(substring(CanadaConsumer$Order.Date,7,10), substring(CanadaConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaConsumer_Quantity) <- c("month", "Quantity")
CanadaConsumer_Quantity_TS <- ts(CanadaConsumer_Quantity$Quantity)
plot(CanadaConsumer_Quantity_TS)

CanadaHomeOffice_Quantity <- aggregate(CanadaHomeOffice$Quantity, by=list(paste(substring(CanadaHomeOffice$Order.Date,7,10), substring(CanadaHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaHomeOffice_Quantity) <- c("month", "Quantity")
CanadaHomeOffice_Quantity_TS <- ts(CanadaHomeOffice_Quantity$Quantity)
plot(CanadaHomeOffice_Quantity_TS)

# EMEA

EMEACorporate_Quantity <- aggregate(EMEACorporate$Quantity, by=list(paste(substring(EMEACorporate$Order.Date,7,10), substring(EMEACorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEACorporate_Quantity) <- c("month", "Quantity")
EMEACorporate_Quantity_TS <- ts(EMEACorporate_Quantity$Quantity)
plot(EMEACorporate_Quantity_TS)

EMEAConsumer_Quantity <- aggregate(EMEAConsumer$Quantity, by=list(paste(substring(EMEAConsumer$Order.Date,7,10), substring(EMEAConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEAConsumer_Quantity) <- c("month", "Quantity")
EMEAConsumer_Quantity_TS <- ts(EMEAConsumer_Quantity$Quantity)
plot(EMEAConsumer_Quantity_TS)

EMEAHomeOffice_Quantity <- aggregate(EMEAHomeOffice$Quantity, by=list(paste(substring(EMEAHomeOffice$Order.Date,7,10), substring(EMEAHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEAHomeOffice_Quantity) <- c("month", "Quantity")
EMEAHomeOffice_Quantity_TS <- ts(EMEAHomeOffice_Quantity$Quantity)
plot(EMEAHomeOffice_Quantity_TS)

# EU

EUCorporate_Quantity <- aggregate(EUCorporate$Quantity, by=list(paste(substring(EUCorporate$Order.Date,7,10), substring(EUCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUCorporate_Quantity) <- c("month", "Quantity")
EUCorporate_Quantity_TS <- ts(EUCorporate_Quantity$Quantity)
plot(EUCorporate_Quantity_TS)

EUConsumer_Quantity <- aggregate(EUConsumer$Quantity, by=list(paste(substring(EUConsumer$Order.Date,7,10), substring(EUConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUConsumer_Quantity) <- c("month", "Quantity")
EUConsumer_Quantity_TS <- ts(EUConsumer_Quantity$Quantity)
plot(EUConsumer_Quantity_TS)

EUHomeOffice_Quantity <- aggregate(EUHomeOffice$Quantity, by=list(paste(substring(EUHomeOffice$Order.Date,7,10), substring(EUHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUHomeOffice_Quantity) <- c("month", "Quantity")
EUHomeOffice_Quantity_TS <- ts(EUHomeOffice_Quantity$Quantity)
plot(EUHomeOffice_Quantity_TS)

# LATAM

LATAMCorporate_Quantity <- aggregate(LATAMCorporate$Quantity, by=list(paste(substring(LATAMCorporate$Order.Date,7,10), substring(LATAMCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMCorporate_Quantity) <- c("month", "Quantity")
LATAMCorporate_Quantity_TS <- ts(LATAMCorporate_Quantity$Quantity)
plot(LATAMCorporate_Quantity_TS)

LATAMConsumer_Quantity <- aggregate(LATAMConsumer$Quantity, by=list(paste(substring(LATAMConsumer$Order.Date,7,10), substring(LATAMConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMConsumer_Quantity) <- c("month", "Quantity")
LATAMConsumer_Quantity_TS <- ts(LATAMConsumer_Quantity$Quantity)
plot(LATAMConsumer_Quantity_TS)

LATAMHomeOffice_Quantity <- aggregate(LATAMHomeOffice$Quantity, by=list(paste(substring(LATAMHomeOffice$Order.Date,7,10), substring(LATAMHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMHomeOffice_Quantity) <- c("month", "Quantity")
LATAMHomeOffice_Quantity_TS <- ts(LATAMHomeOffice_Quantity$Quantity)
plot(LATAMHomeOffice_Quantity_TS)

# US

USCorporate_Quantity <- aggregate(USCorporate$Quantity, by=list(paste(substring(USCorporate$Order.Date,7,10), substring(USCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USCorporate_Quantity) <- c("month", "Quantity")
USCorporate_Quantity_TS <- ts(USCorporate_Quantity$Quantity)
plot(USCorporate_Quantity_TS)

USConsumer_Quantity <- aggregate(USConsumer$Quantity, by=list(paste(substring(USConsumer$Order.Date,7,10), substring(USConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USConsumer_Quantity) <- c("month", "Quantity")
USConsumer_Quantity_TS <- ts(USConsumer_Quantity$Quantity)
plot(USConsumer_Quantity_TS)

USHomeOffice_Quantity <- aggregate(USHomeOffice$Quantity, by=list(paste(substring(USHomeOffice$Order.Date,7,10), substring(USHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USHomeOffice_Quantity) <- c("month", "Quantity")
USHomeOffice_Quantity_TS <- ts(USHomeOffice_Quantity$Quantity)
plot(USHomeOffice_Quantity_TS)

# PROFIT

# Africa

AfricaCorporate_Profit <- aggregate(AfricaCorporate$Profit, by=list(paste(substring(AfricaCorporate$Order.Date,7,10), substring(AfricaCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaCorporate_Profit) <- c("month", "Profit")
AfricaCorporate_Profit_TS <- ts(AfricaCorporate_Profit$Profit)
plot(AfricaCorporate_Profit_TS)

AfricaConsumer_Profit <- aggregate(AfricaConsumer$Profit, by=list(paste(substring(AfricaConsumer$Order.Date,7,10), substring(AfricaConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaConsumer_Profit) <- c("month", "Profit")
AfricaConsumer_Profit_TS <- ts(AfricaConsumer_Profit$Profit)
plot(AfricaConsumer_Profit_TS)

AfricaHomeOffice_Profit <- aggregate(AfricaHomeOffice$Profit, by=list(paste(substring(AfricaHomeOffice$Order.Date,7,10), substring(AfricaHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(AfricaHomeOffice_Profit) <- c("month", "Profit")
AfricaHomeOffice_Profit_TS <- ts(AfricaHomeOffice_Profit$Profit)
plot(AfricaHomeOffice_Profit_TS)

# APAC

APACCorporate_Profit <- aggregate(APACCorporate$Profit, by=list(paste(substring(APACCorporate$Order.Date,7,10), substring(APACCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACCorporate_Profit) <- c("month", "Profit")
APACCorporate_Profit_TS <- ts(APACCorporate_Profit$Profit)
plot(APACCorporate_Profit_TS)

APACConsumer_Profit <- aggregate(APACConsumer$Profit, by=list(paste(substring(APACConsumer$Order.Date,7,10), substring(APACConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACConsumer_Profit) <- c("month", "Profit")
APACConsumer_Profit_TS <- ts(APACConsumer_Profit$Profit)
plot(APACConsumer_Profit_TS)

APACHomeOffice_Profit <- aggregate(APACHomeOffice$Profit, by=list(paste(substring(APACHomeOffice$Order.Date,7,10), substring(APACHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(APACHomeOffice_Profit) <- c("month", "Profit")
APACHomeOffice_Profit_TS <- ts(APACHomeOffice_Profit$Profit)
plot(APACHomeOffice_Profit_TS)

# Canada

CanadaCorporate_Profit <- aggregate(CanadaCorporate$Profit, by=list(paste(substring(CanadaCorporate$Order.Date,7,10), substring(CanadaCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaCorporate_Profit) <- c("month", "Profit")
CanadaCorporate_Profit_TS <- ts(CanadaCorporate_Profit$Profit)
plot(CanadaCorporate_Profit_TS)

CanadaConsumer_Profit <- aggregate(CanadaConsumer$Profit, by=list(paste(substring(CanadaConsumer$Order.Date,7,10), substring(CanadaConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaConsumer_Profit) <- c("month", "Profit")
CanadaConsumer_Profit_TS <- ts(CanadaConsumer_Profit$Profit)
plot(CanadaConsumer_Profit_TS)

CanadaHomeOffice_Profit <- aggregate(CanadaHomeOffice$Profit, by=list(paste(substring(CanadaHomeOffice$Order.Date,7,10), substring(CanadaHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(CanadaHomeOffice_Profit) <- c("month", "Profit")
CanadaHomeOffice_Profit_TS <- ts(CanadaHomeOffice_Profit$Profit)
plot(CanadaHomeOffice_Profit_TS)

# EMEA

EMEACorporate_Profit <- aggregate(EMEACorporate$Profit, by=list(paste(substring(EMEACorporate$Order.Date,7,10), substring(EMEACorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEACorporate_Profit) <- c("month", "Profit")
EMEACorporate_Profit_TS <- ts(EMEACorporate_Profit$Profit)
plot(EMEACorporate_Profit_TS)

EMEAConsumer_Profit <- aggregate(EMEAConsumer$Profit, by=list(paste(substring(EMEAConsumer$Order.Date,7,10), substring(EMEAConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEAConsumer_Profit) <- c("month", "Profit")
EMEAConsumer_Profit_TS <- ts(EMEAConsumer_Profit$Profit)
plot(EMEAConsumer_Profit_TS)

EMEAHomeOffice_Profit <- aggregate(EMEAHomeOffice$Profit, by=list(paste(substring(EMEAHomeOffice$Order.Date,7,10), substring(EMEAHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EMEAHomeOffice_Profit) <- c("month", "Profit")
EMEAHomeOffice_Profit_TS <- ts(EMEAHomeOffice_Profit$Profit)
plot(EMEAHomeOffice_Profit_TS)

# EU

EUCorporate_Profit <- aggregate(EUCorporate$Profit, by=list(paste(substring(EUCorporate$Order.Date,7,10), substring(EUCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUCorporate_Profit) <- c("month", "Profit")
EUCorporate_Profit_TS <- ts(EUCorporate_Profit$Profit)
plot(EUCorporate_Profit_TS)

EUConsumer_Profit <- aggregate(EUConsumer$Profit, by=list(paste(substring(EUConsumer$Order.Date,7,10), substring(EUConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUConsumer_Profit) <- c("month", "Profit")
EUConsumer_Profit_TS <- ts(EUConsumer_Profit$Profit)
plot(EUConsumer_Profit_TS)

EUHomeOffice_Profit <- aggregate(EUHomeOffice$Profit, by=list(paste(substring(EUHomeOffice$Order.Date,7,10), substring(EUHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(EUHomeOffice_Profit) <- c("month", "Profit")
EUHomeOffice_Profit_TS <- ts(EUHomeOffice_Profit$Profit)
plot(EUHomeOffice_Profit_TS)

# LATAM

LATAMCorporate_Profit <- aggregate(LATAMCorporate$Profit, by=list(paste(substring(LATAMCorporate$Order.Date,7,10), substring(LATAMCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMCorporate_Profit) <- c("month", "Profit")
LATAMCorporate_Profit_TS <- ts(LATAMCorporate_Profit$Profit)
plot(LATAMCorporate_Profit_TS)

LATAMConsumer_Profit <- aggregate(LATAMConsumer$Profit, by=list(paste(substring(LATAMConsumer$Order.Date,7,10), substring(LATAMConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMConsumer_Profit) <- c("month", "Profit")
LATAMConsumer_Profit_TS <- ts(LATAMConsumer_Profit$Profit)
plot(LATAMConsumer_Profit_TS)

LATAMHomeOffice_Profit <- aggregate(LATAMHomeOffice$Profit, by=list(paste(substring(LATAMHomeOffice$Order.Date,7,10), substring(LATAMHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(LATAMHomeOffice_Profit) <- c("month", "Profit")
LATAMHomeOffice_Profit_TS <- ts(LATAMHomeOffice_Profit$Profit)
plot(LATAMHomeOffice_Profit_TS)

# US

USCorporate_Profit <- aggregate(USCorporate$Profit, by=list(paste(substring(USCorporate$Order.Date,7,10), substring(USCorporate$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USCorporate_Profit) <- c("month", "Profit")
USCorporate_Profit_TS <- ts(USCorporate_Profit$Profit)
plot(USCorporate_Profit_TS)

USConsumer_Profit <- aggregate(USConsumer$Profit, by=list(paste(substring(USConsumer$Order.Date,7,10), substring(USConsumer$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USConsumer_Profit) <- c("month", "Profit")
USConsumer_Profit_TS <- ts(USConsumer_Profit$Profit)
plot(USConsumer_Profit_TS)

USHomeOffice_Profit <- aggregate(USHomeOffice$Profit, by=list(paste(substring(USHomeOffice$Order.Date,7,10), substring(USHomeOffice$Order.Date,4,5), sep = "-")), FUN = sum)
colnames(USHomeOffice_Profit) <- c("month", "Profit")
USHomeOffice_Profit_TS <- ts(USHomeOffice_Profit$Profit)
plot(USHomeOffice_Profit_TS)


## Coeficient Of Variation [(std dev/mean)*100]
# Example:
# If the SPDR S&P 500 ETF has an average annual return of 5.47% and a standard deviation of 14.68%, # the SPDR S&P 500 ETF's coefficient of variation is 2.68.


m = mean(AfricaCorporate_Profit$Profit)
s = sd(AfricaCorporate_Profit$Profit)
CV_AfricaCorporate = (s/m)*100
CV_AfricaCorporate

m = mean(AfricaConsumer_Profit$Profit)
s = sd(AfricaConsumer_Profit$Profit)
CV_AfricaConsumer = (s/m)*100
CV_AfricaConsumer

m = mean(AfricaHomeOffice_Profit$Profit)
s = sd(AfricaHomeOffice_Profit$Profit)
CV_AfricaHomeOffice = (s/m)*100
CV_AfricaHomeOffice


m = mean(APACCorporate_Profit$Profit)
s = sd(APACCorporate_Profit$Profit)
CV_APACCorporate = (s/m)*100
CV_APACCorporate

m = mean(APACConsumer_Profit$Profit)
s = sd(APACConsumer_Profit$Profit)
CV_APACConsumer = (s/m)*100
CV_APACConsumer

m = mean(APACHomeOffice_Profit$Profit)
s = sd(APACHomeOffice_Profit$Profit)
CV_APACHomeOffice= (s/m)*100
CV_APACHomeOffice

m = mean(CanadaCorporate_Profit$Profit)
s = sd(CanadaCorporate_Profit$Profit)
CV_CanadaCorporate = (s/m)*100
CV_CanadaCorporate

m = mean(CanadaConsumer_Profit$Profit)
s = sd(CanadaConsumer_Profit$Profit)
CV_CanadaConsumer = (s/m)*100
CV_CanadaConsumer

m = mean(CanadaHomeOffice_Profit$Profit)
s = sd(CanadaHomeOffice_Profit$Profit)
CV_CanadaHomeOffice = (s/m)*100
CV_CanadaHomeOffice

m = mean(EMEACorporate_Profit$Profit)
s = sd(EMEACorporate_Profit$Profit)
CV_EMEACorporate = (s/m)*100
CV_EMEACorporate

m = mean(EMEAConsumer_Profit$Profit)
s = sd(EMEAConsumer_Profit$Profit)
CV_EMEAConsumer = (s/m)*100
CV_EMEAConsumer

m = mean(EMEAHomeOffice_Profit$Profit)
s = sd(EMEAHomeOffice_Profit$Profit)
CV_EMEAHomeOffice = (s/m)*100
CV_EMEAHomeOffice

m = mean(EUCorporate_Profit$Profit)
s = sd(EUCorporate_Profit$Profit)
CV_EUCorporate = (s/m)*100
CV_EUCorporate

m = mean(EUConsumer_Profit$Profit)
s = sd(EUConsumer_Profit$Profit)
CV_EUConsumer = (s/m)*100
CV_EUConsumer

m = mean(EUHomeOffice_Profit$Profit)
s = sd(EUHomeOffice_Profit$Profit)
CV_EUHomeOffice = (s/m)*100
CV_EUHomeOffice

m = mean(LATAMCorporate_Profit$Profit)
s = sd(LATAMCorporate_Profit$Profit)
CV_LATAMCorporate = (s/m)*100
CV_LATAMCorporate

m = mean(LATAMConsumer_Profit$Profit)
s = sd(LATAMConsumer_Profit$Profit)
CV_LATAMConsumer = (s/m)*100
CV_LATAMConsumer

m = mean(LATAMHomeOffice_Profit$Profit)
s = sd(LATAMHomeOffice_Profit$Profit)
CV_LATAMHomeOffice = (s/m)*100
CV_LATAMHomeOffice

m = mean(USCorporate_Profit$Profit)
s = sd(USCorporate_Profit$Profit)
CV_USCorporate = (s/m)*100
CV_USCorporate

m = mean(USConsumer_Profit$Profit)
s = sd(USConsumer_Profit$Profit)
CV_USConsumer = (s/m)*100
CV_USConsumer

m = mean(USHomeOffice_Profit$Profit)
s = sd(USHomeOffice_Profit$Profit)
CV_USHomeOffice = (s/m)*100
CV_USHomeOffice

sum(USHomeOffice_Profit$Profit)
CV_USHomeOffice

sum(USConsumer_Profit$Profit)
CV_USConsumer

sum(USCorporate_Profit$Profit)
CV_USCorporate

sum(LATAMHomeOffice_Profit$Profit)
CV_LATAMHomeOffice

sum(LATAMConsumer_Profit$Profit)
CV_LATAMConsumer

sum(LATAMCorporate_Profit$Profit)
CV_LATAMCorporate

sum(EUHomeOffice_Profit$Profit)
CV_EUHomeOffice

sum(EUConsumer_Profit$Profit)
CV_EUConsumer

sum(EUCorporate_Profit$Profit)
CV_EUCorporate

sum(EMEAHomeOffice_Profit$Profit)
CV_EMEAHomeOffice

sum(EMEAConsumer_Profit$Profit)
CV_EMEAConsumer

sum(EMEACorporate_Profit$Profit)
CV_EMEACorporate

sum(CanadaHomeOffice_Profit$Profit)
CV_CanadaHomeOffice

sum(CanadaConsumer_Profit$Profit)
CV_CanadaConsumer

sum(CanadaCorporate_Profit$Profit)
CV_CanadaCorporate

sum(APACHomeOffice_Profit$Profit)
CV_APACHomeOffice

sum(APACConsumer_Profit$Profit)
CV_APACConsumer

sum(APACCorporate_Profit$Profit)
CV_APACCorporate

sum(AfricaHomeOffice_Profit$Profit)
CV_AfricaHomeOffice

sum(AfricaConsumer_Profit$Profit)
CV_AfricaConsumer

sum(AfricaCorporate_Profit$Profit)
CV_AfricaCorporate

# Top Profitable segments in terms of aggregate profit (rounded figures):

# APAC Consumer : 222k ; 63
#   EU Consumer : 188k ; 62.4
# APAC Corporate: 129k ; 69
#  EU Corporate : 123k ; 76
# LATAM Consumer: 120k ; 66
# US Consumer   : 134k ; 101
# US Corporate  : 92k  ; 100


# Top 5 Segments balancing Profit and Coefficient of Variation :
############ segments selected for further study ############

# APAC Consumer 
#   EU Consumer 
# APAC Corporate
#  EU Corporate 
# LATAM Consumer

################ Time Series Analysis of Sales for CONSUMER Segment in APAC marketplace #################

#removing latest 6 months data from the dataset and storing it into train_APACConsumer_Sales
train_APACConsumer_Sales <- head(APACConsumer_Sales, -6)
#storing latest 6 months data to test dataset
test_APACConsumer_Sales <- tail(APACConsumer_Sales, 6)


timeseries <- ts(train_APACConsumer_Sales$sales)
plot(timeseries, ylab="Sales",xlab="Month Number",col="blue")

# Plotly TS
library(plotly)
fig <- plot_ly(y = ~train_APACConsumer_Sales$sales, x = ~train_APACConsumer_Sales$monthOrder, mode = 'lines', text = "Month Number")

fig

###


# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for APAC Consumer Sales")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for APAC Consumer Sales")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

library(stats)
plot(timeseries, ylab="Sales",xlab="Month Number",col="blue")
w <- 1
timeseries_smoothed <- stats::filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries , ylab="Sales",xlab="Month Number",col="blue")
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_APACConsumer_Sales$monthOrder <- seq.int(nrow(train_APACConsumer_Sales[1]))
test_APACConsumer_Sales$monthOrder <- seq.int(nrow(test_APACConsumer_Sales[1]))

lmfit <- lm(train_APACConsumer_Sales$sales ~ sin(0.3*train_APACConsumer_Sales$monthOrder)*poly(train_APACConsumer_Sales$monthOrder, 4) + cos(0.3*train_APACConsumer_Sales$monthOrder)*poly(train_APACConsumer_Sales$monthOrder, 4) + train_APACConsumer_Sales$monthOrder, data = train_APACConsumer_Sales)
trend <- predict(lmfit, data = train_APACConsumer_Sales$monthOrder)
lines(train_APACConsumer_Sales$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(0,0,0) with zero mean     
# sigma^2 estimated as 83420737:  log likelihood=-442.62
# AIC=887.25   AICc=887.35   BIC=888.98
# p,d,q : (0,0,0)

# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(0,1,1)                    
# sigma^2 estimated as 174361555:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66
# p,d,q : (0,1,1)

# Autoarima plot overlayed on original time series
plot(timeseries, ylab="Sales",xlab="Month Number",col="blue")
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE
require(forecast)

linear_forecast_1 <- forecast(trend, 6, level = 0)
linear_forecast_1
plot.ts(linear_forecast_1)

linear_acc <- accuracy(linear_forecast$mean, test_APACConsumer_Sales$sales)
linear_acc
# MAPE for Regression : 28.68488


linear_forecast <- forecast(autoarima, 6)
linear_forecast
plot.ts(linear_forecast)

linear_acc <- accuracy(linear_forecast$mean, test_APACConsumer_Sales$sales)
linear_acc
# MAPE for autoarima :  27.68952


################ Time Series Analysis of Quantity for CONSUMER Segment in APAC marketplace ########

train_APACConsumer_Quantity <- head(APACConsumer_Quantity, -6)
test_APACConsumer_Quantity <- tail(APACConsumer_Quantity, 6)

timeseries <- ts(train_APACConsumer_Quantity$Quantity)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for APAC Consumer Quantity")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for APAC Consumer Quantity")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 1
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_APACConsumer_Quantity$monthOrder <- seq.int(nrow(train_APACConsumer_Quantity[1]))
test_APACConsumer_Quantity$monthOrder <- seq.int(nrow(test_APACConsumer_Quantity[1]))

lmfit <- lm(train_APACConsumer_Quantity$Quantity ~ sin(0.3*train_APACConsumer_Quantity$monthOrder)*poly(train_APACConsumer_Quantity$monthOrder, 4) + cos(0.3*train_APACConsumer_Quantity$monthOrder)*poly(train_APACConsumer_Quantity$monthOrder, 4) + train_APACConsumer_Quantity$monthOrder, data = train_APACConsumer_Quantity)
trend <- predict(lmfit, data = train_APACConsumer_Quantity$monthOrder)
lines(train_APACConsumer_Quantity$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(0,0,0) with zero mean     
# sigma^2 estimated as 9608:  log likelihood=-252.17
# AIC=506.34   AICc=506.44   BIC=508.08
# p,d,q : (0,0,0)

# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(0,1,0)                   
# sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85
# p,d,q : (0,1,0)

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression : 37.13402

linear_forecast_2 <- forecast(trend, 6, level = 0)
linear_forecast_2
plot.ts(linear_forecast_2)

linear_acc <- accuracy(linear_forecast$mean, test_APACConsumer_Quantity$Quantity)
linear_acc

# MAPE for autoarima : 26.24458
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast$mean, test_APACConsumer_Quantity$Quantity)
linear_acc


################ Time Series Analysis of Sales for Consumer Segment in EU marketplace #################

train_EUConsumer_Sales <- head(EUConsumer_Sales, -6)
test_EUConsumer_Sales <- tail(EUConsumer_Sales, 6)

timeseries <- ts(train_EUConsumer_Sales$sales)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for EU Consumer Sales")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for EU Consumer Sales")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 2
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_EUConsumer_Sales$monthOrder <- seq.int(nrow(train_EUConsumer_Sales[1]))
test_EUConsumer_Sales$monthOrder <- seq.int(nrow(test_EUConsumer_Sales[1]))

lmfit <- lm(train_EUConsumer_Sales$sales ~ sin(0.5*train_EUConsumer_Sales$monthOrder)*poly(train_EUConsumer_Sales$monthOrder, 2) + cos(0.5*train_EUConsumer_Sales$monthOrder)*poly(train_EUConsumer_Sales$monthOrder, 2) + train_EUConsumer_Sales$monthOrder, data = train_EUConsumer_Sales)
trend <- predict(lmfit, data = train_EUConsumer_Sales$monthOrder)
lines(train_EUConsumer_Sales$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(0,0,2) with non-zero mean 
# sigma^2 estimated as 82883782:  log likelihood=-441.24
# AIC=890.49   AICc=891.57   BIC=897.44
# p,d,q : (0,0,2)

# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(2,1,0)                    
# sigma^2 estimated as 168564623:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81
# p,d,q : (2,1,0)

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression :  22.71581
require(forecast)

linear_forecast <- forecast(trend, 6, level = 0)
linear_forecast
plot.ts(linear_forecast)

linear_acc <- accuracy(linear_forecast$mean, test_EUConsumer_Sales$sales)
linear_acc

# MAPE for autoarima : 28.9226
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast$mean, test_EUConsumer_Sales$sales)
linear_acc


################ Time Series Analysis of Quantity for Consumer Segment in EU marketplace ###########

train_EUConsumer_Quantity <- head(EUConsumer_Quantity, -6)
test_EUConsumer_Quantity <- tail(EUConsumer_Quantity, 6)

timeseries <- ts(train_EUConsumer_Quantity$Quantity)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for EU Consumer Quantity")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for EU Consumer Quantity")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 2
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_EUConsumer_Quantity$monthOrder <- seq.int(nrow(train_EUConsumer_Quantity[1]))
test_EUConsumer_Quantity$monthOrder <- seq.int(nrow(test_EUConsumer_Quantity[1]))

lmfit <- lm(train_EUConsumer_Quantity$Quantity ~ sin(0.3*train_EUConsumer_Quantity$monthOrder)*poly(train_EUConsumer_Quantity$monthOrder, 4) + cos(0.3*train_EUConsumer_Quantity$monthOrder)*poly(train_EUConsumer_Quantity$monthOrder, 4) + train_EUConsumer_Quantity$monthOrder, data = train_EUConsumer_Quantity)
trend <- predict(lmfit, data = train_EUConsumer_Quantity$monthOrder)
lines(train_EUConsumer_Quantity$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(2,0,0) with zero mean     
# ssigma^2 estimated as 7051:  log likelihood=-245.23
# AIC=496.46   AICc=497.1   BIC=501.68
# p,d,q : (2,0,0)


# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(2,1,0)                    
# sigma^2 estimated as 21185:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94
# p,d,q : (2,1,0) 

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression : 29.37811

linear_forecast <- forecast(trend, 6, level = 0)
linear_forecast
plot.ts(linear_forecast)
linear_acc <- accuracy(linear_forecast$mean, test_EUConsumer_Quantity$Quantity)
linear_acc

# MAPE for autoarima : 30.13319
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast$mean, test_EUConsumer_Quantity$Quantity)
linear_acc

################ Time Series Analysis of Sales for CORPORATE Segment in APAC marketplace ###############

train_APACCorporate_Sales <- head(APACCorporate_Sales, -6)
test_APACCorporate_Sales <- tail(APACCorporate_Sales, 6)

timeseries <- ts(train_APACCorporate_Sales$sales)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for APAC Corporate Sales")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for APAC Corporate Sales")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 1
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_APACCorporate_Sales$monthOrder <- seq.int(nrow(train_APACCorporate_Sales[1]))
test_APACCorporate_Sales$monthOrder <- seq.int(nrow(test_APACCorporate_Sales[1]))

lmfit <- lm(train_APACCorporate_Sales$sales ~ sin(0.3*train_APACCorporate_Sales$monthOrder)*poly(train_APACCorporate_Sales$monthOrder, 4) + cos(0.3*train_APACCorporate_Sales$monthOrder)*poly(train_APACCorporate_Sales$monthOrder, 4) + train_APACCorporate_Sales$monthOrder, data = train_APACCorporate_Sales)
trend <- predict(lmfit, data = train_APACCorporate_Sales$monthOrder)
lines(train_APACCorporate_Sales$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(0,0,0) with zero mean     
# sigma^2 estimated as 56655352:  log likelihood=-434.5
# AIC=871   AICc=871.1   BIC=872.73
# p,d,q : (0,0,0)


# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(0,1,1)                    
# sigma^2 estimated as 101430783:  log likelihood=-436.32
# AIC=876.64   AICc=876.96   BIC=880.07
# p,d,q : (0,1,1)

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression : 26.55771
require(forecast)

linear_forecast <- forecast(trend, 6, level = 0)
linear_forecast
plot.ts(linear_forecast)
linear_acc <- accuracy(linear_forecast$mean, test_APACCorporate_Sales$sales)
linear_acc

# MAPE for autoarima : 27.97408
linear_forecast <- forecast(autoarima, 6)
linear_forecast

linear_acc <- accuracy(linear_forecast$mean, test_APACCorporate_Sales$sales)
linear_acc



################ Time Series Analysis of Quantity for CORPORATE Segment in APAC marketplace ###########

train_APACCorporate_Quantity <- head(APACCorporate_Quantity, -6)
test_APACCorporate_Quantity <- tail(APACCorporate_Quantity, 6)

timeseries <- ts(train_APACCorporate_Quantity$Quantity)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for APAC Corporate Quantity")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for APAC Corporate Quantity")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 1
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_APACCorporate_Quantity$monthOrder <- seq.int(nrow(train_APACCorporate_Quantity[1]))
test_APACCorporate_Quantity$monthOrder <- seq.int(nrow(test_APACCorporate_Quantity[1]))

lmfit <- lm(train_APACCorporate_Quantity$Quantity ~ sin(0.5*train_APACCorporate_Quantity$monthOrder)*poly(train_APACCorporate_Quantity$monthOrder, 5) + cos(0.5*train_APACCorporate_Quantity$monthOrder)*poly(train_APACCorporate_Quantity$monthOrder, 5) + train_APACCorporate_Quantity$monthOrder, data = train_APACCorporate_Quantity)
trend <- predict(lmfit, data = train_APACCorporate_Quantity$monthOrder)
lines(train_APACCorporate_Quantity$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(5,0,0) with zero mean     
# sigma^2 estimated as 1457:  log likelihood=-211.58
# AIC=435.16   AICc=437.56   BIC=445.58
# p,d,q : (5,0,0)


# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(0,1,1)                    
# sigma^2 estimated as 11945:  log likelihood=-250.71
# AIC=505.41   AICc=505.73   BIC=508.84
# p,d,q : (0,1,1)

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression : 27.67883

linear_forecast <- forecast(trend, 6)
linear_forecast
plot.ts(linear_forecast)

linear_acc <- accuracy(linear_forecast$mean, test_APACCorporate_Quantity$Quantity)
linear_acc

# MAPE for autoarima : 24.13219
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast$mean, test_APACCorporate_Quantity$Quantity)
linear_acc


################ Time Series Analysis of Sales for CORPORATE Segment in EU marketplace #################

train_EUCorporate_Sales <- head(EUCorporate_Sales, -6)
test_EUCorporate_Sales <- tail(EUCorporate_Sales, 6)

timeseries <- ts(train_EUCorporate_Sales$sales)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for EU Corporate Sales")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for EU Corporate Sales")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 0.5
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_EUCorporate_Sales$monthOrder <- seq.int(nrow(train_EUCorporate_Sales[1]))
test_EUCorporate_Sales$monthOrder <- seq.int(nrow(test_EUCorporate_Sales[1]))

lmfit <- lm(train_EUCorporate_Sales$sales ~ sin(0.5*train_EUCorporate_Sales$monthOrder)*poly(train_EUCorporate_Sales$monthOrder, 2) + cos(0.5*train_EUCorporate_Sales$monthOrder)*poly(train_EUCorporate_Sales$monthOrder, 2) + train_APACCorporate_Sales$monthOrder, data = train_EUCorporate_Sales)
trend <- predict(lmfit, data = train_EUCorporate_Sales$monthOrder)
lines(train_EUCorporate_Sales$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(2,0,0) with zero mean     
# sigma^2 estimated as 32403497:  log likelihood=-421.97
# AIC=849.93   AICc=850.56   BIC=855.15
# p,d,q : (2,0,0)

# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(2,1,0)                    
# sigma^2 estimated as 70129310:  log likelihood=-428.01
# AIC=862.03   AICc=862.68   BIC=867.17
# p,d,q : (2,1,0) 

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression : 79.79463

linear_forecast <- forecast(trend, 6, level = 0)
linear_forecast
plot.ts(linear_forecast)
linear_acc <- accuracy(linear_forecast$mean, test_EUCorporate_Sales$sales)
linear_acc

# MAPE for autoarima : 36.35092
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast$mean, test_EUCorporate_Sales$sales)
linear_acc


################ Time Series Analysis of Quantity for CORPORATE Segment in EU marketplace ###############

train_EUCorporate_Quantity <- head(EUCorporate_Quantity, -6)
test_EUCorporate_Quantity <- tail(EUCorporate_Quantity, 6)

timeseries <- ts(train_EUCorporate_Quantity$Quantity)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for EU Corporate Quantity")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for EU Corporate Quantity")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 2
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_EUCorporate_Quantity$monthOrder <- seq.int(nrow(train_EUCorporate_Quantity[1]))
test_EUCorporate_Quantity$monthOrder <- seq.int(nrow(test_EUCorporate_Quantity[1]))

lmfit <- lm(train_EUCorporate_Quantity$Quantity ~ sin(0.3*train_EUCorporate_Quantity$monthOrder)*poly(train_EUCorporate_Quantity$monthOrder, 2) + cos(0.3*train_EUCorporate_Quantity$monthOrder)*poly(train_EUCorporate_Quantity$monthOrder, 2) + train_EUCorporate_Quantity$monthOrder, data = train_EUCorporate_Quantity)
trend <- predict(lmfit, data = train_EUCorporate_Quantity$monthOrder)
lines(train_EUCorporate_Quantity$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(2,0,0) with zero mean     
# sigma^2 estimated as 4441:  log likelihood=-235.09
# AIC=476.18   AICc=476.81   BIC=481.39
# p,d,q : (2,0,0)

# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(2,1,0)                    
# sigma^2 estimated as 9869:  log likelihood=-246.12
# AIC=498.25   AICc=498.9   BIC=503.39
# p,d,q : (2,1,0)

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression : 197.4539
require(forecast)

linear_forecast <- forecast(trend, 6, level = 0)
linear_forecast
plot.ts(linear_forecast)
linear_acc <- accuracy(linear_forecast$mean, test_EUCorporate_Quantity$Quantity)
linear_acc

# MAPE for autoarima : 47.54968
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast$mean, test_EUCorporate_Quantity$Quantity)
linear_acc


################ Time Series Analysis of Sales for Consumer Segment in LATAM marketplace #############

train_LATAMConsumer_Sales <- head(LATAMConsumer_Sales, -6)
test_LATAMConsumer_Sales <- tail(LATAMConsumer_Sales, 6)

timeseries <- ts(train_LATAMConsumer_Sales$sales)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for LATAM Consumer Sales")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for LATAM Consumer Sales")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 2
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_LATAMConsumer_Sales$monthOrder <- seq.int(nrow(train_LATAMConsumer_Sales[1]))
test_LATAMConsumer_Sales$monthOrder <- seq.int(nrow(test_LATAMConsumer_Sales[1]))

lmfit <- lm(train_LATAMConsumer_Sales$sales ~ sin(0.3*train_LATAMConsumer_Sales$monthOrder)*poly(train_LATAMConsumer_Sales$monthOrder, 4) + cos(0.3*train_LATAMConsumer_Sales$monthOrder)*poly(train_LATAMConsumer_Sales$monthOrder, 4) + train_LATAMConsumer_Sales$monthOrder, data = train_LATAMConsumer_Sales)
trend <- predict(lmfit, data = train_LATAMConsumer_Sales$monthOrder)
lines(train_LATAMConsumer_Sales$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(0,0,0) with zero mean     
# sigma^2 estimated as 43934968:  log likelihood=-429.16
# AIC=860.32   AICc=860.42   BIC=862.05
# p,d,q : (0,0,0)

# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(0,1,0)                    
# sigma^2 estimated as 119534145:  log likelihood=-439.46
# AIC=880.92   AICc=881.02   BIC=882.63
# p,d,q : (0,1,0)

# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 

# MAPE for Regression : 31.66988
require(forecast)

linear_forecast <- forecast(trend, 6, level = 0)
linear_forecast
plot.ts(linear_forecast)
linear_acc <- accuracy(linear_forecast$mean, test_LATAMConsumer_Sales$sales)
linear_acc

# MAPE for autoarima : 33.96611
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast$mean, test_LATAMConsumer_Sales$sales)
linear_acc


################ Time Series Analysis of Quantity for Consumer Segment in LATAM marketplace ############

train_LATAMConsumer_Quantity <- head(LATAMConsumer_Quantity, -6)
test_LATAMConsumer_Quantity <- tail(LATAMConsumer_Quantity, 6)

timeseries <- ts(train_LATAMConsumer_Quantity$Quantity)
plot(timeseries)

# Plot shows us a slow upward moving trend with lots of ups and downs

acf(timeseries, level = 95, lag.max = 30, main = "ACF Plot for LATAM Consumer Quantity")
pacf(timeseries, level = 95, lag.max = 30, main = "PACF Plot for LATAM Consumer Quantity")

####### Analysis of time series - Manual decomposition and Auto ARIMA
####### Manual Decomposition 
# Smoothing technique used : Convolution (Moving Average)
# filter(window) = 2*w+1 is fitted in this method

plot(timeseries)
w <- 2
timeseries_smoothed <- filter(timeseries, filter = rep(1/(2*w+1),(2*w+1)), method = "convolution", sides = 2)

# extrapolating both ends of the series as they couldn't be captured in the moving window above
diffr <- timeseries_smoothed[w+2] - timeseries_smoothed[w+1]
for(i in seq(w,1,-1)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i+1] - diffr
}
n <- length(timeseries_smoothed)
diffr <- timeseries_smoothed[n-1] - timeseries_smoothed[n-w-1]
for(i in seq(n-w+1, n)) {
  timeseries_smoothed[i] <- timeseries_smoothed[i-1] + diffr
}

# Plot of the Smoothed series
plot(timeseries_smoothed, col = "green", lwd = 2)

# Overlayed plot of smoothed series
plot(timeseries)
lines(timeseries_smoothed, col = "green", lwd = 2)

#### Fitting the TREND LINE / Regression line

train_LATAMConsumer_Quantity$monthOrder <- seq.int(nrow(train_LATAMConsumer_Quantity[1]))
test_LATAMConsumer_Quantity$monthOrder <- seq.int(nrow(test_LATAMConsumer_Quantity[1]))

lmfit <- lm(train_LATAMConsumer_Quantity$Quantity ~ sin(0.3*train_LATAMConsumer_Quantity$monthOrder)*poly(train_LATAMConsumer_Quantity$monthOrder, 4) + cos(0.3*train_LATAMConsumer_Quantity$monthOrder)*poly(train_LATAMConsumer_Quantity$monthOrder, 4) + train_LATAMConsumer_Quantity$monthOrder, data = train_LATAMConsumer_Quantity)
trend <- predict(lmfit, data = train_LATAMConsumer_Quantity$monthOrder)
lines(train_LATAMConsumer_Quantity$monthOrder , trend, col = 'red' , lwd = 2)

# Residue - Taking the predictable part out of the Time Series leaves us with the residue
resi <- timeseries - trend
plot(resi, col = "red")
acf(resi)
acf(resi, type = "partial")

# ACF and PACF (same as acf with type = partial) plots show many lines nearly 
# or completely out of the confidence interval (dotted blue lines) indicating noise

armafit <- auto.arima(resi)
tsdiag(armafit)
armafit

# ARIMA(0,0,0) with zero mean     
# sigma^2 estimated as 14435:  log likelihood=-260.72
# AIC=523.44   AICc=523.54   BIC=525.18
# p,d,q : (0,0,0)

# Auto ARIMA on original time series
autoarima <- auto.arima(timeseries)
tsdiag(autoarima)
autoarima

# ARIMA(0,1,0)                    
# sigma^2 estimated as 36435:  log likelihood=-273.49
# AIC=548.99   AICc=549.09   BIC=550.7
# p,d,q : (0,1,0)                    


# Autoarima plot overlayed on original time series
plot(timeseries)
lines(fitted(autoarima), col = "red")

## Log likelihood, AIC, AICc, BIC values indicate that manual model is clearly better than the Auto generated one in terms of all the parameters 


# MAPE for Regression : 123.57532

linear_forecast <- forecast(trend, 6)
linear_forecast
plot.ts(linear_forecast)
linear_acc <- accuracy(linear_forecast, test_LATAMConsumer_Quantity$Quantity)
linear_acc

# MAPE for autoarima : 47.69576
linear_forecast <- forecast(autoarima, 6)
linear_forecast
linear_acc <- accuracy(linear_forecast, test_LATAMConsumer_Quantity$Quantity)
linear_acc
