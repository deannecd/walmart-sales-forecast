# Walmart Sales Forecasting
# https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/data

# Predict the department-wide sales for each store

# 45 stores

# Some ideas:
# OLS regression to get impact of promotional Mark Down events
# Time series forecasting per store, adjust for seasonal trends, and external factors
# Random Forest to predict sales per store

library(foreign)
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(RColorBrewer)
library(ggcorrplot)
library(stringr)
library(stargazer)
library(ggfortify)
library(forecast)
library(MLmetrics)
library(xts)
library(zoo)
library(fpp2)
library(TTR)
theme_set(theme_classic())

setwd("C:/Documents/Data Science/R Tutorial/Walmart Sales Forecasting")

dfFeatures <- read_csv("features.csv")
dfStores <- read_csv("stores.csv")
dfTest <- read_csv("test.csv")
dfTrain <- read_csv("train.csv")

# Combine the 4 datasets

# Merge Type, Size, Dept and Sales
dfTrainStore <- merge(x=dfTrain, y=dfStores, all.x=TRUE)
dfTestStore <- merge(x=dfTest, y=dfStores, all.x=TRUE)

# Merge all the Features
dfTrainMerged <- merge(x=dfTrainStore, y=dfFeatures, all.x=TRUE)
dfTestMerged <- merge(x=dfTestStore, y=dfFeatures, all.x=TRUE)

# Save Datasets
write.table(x=dfTrainMerged,
            file='trainMerged.csv',
            sep=',', row.names=FALSE, quote=FALSE)
write.table(x=dfTestMerged,
            file='testMerged.csv',
            sep=',', row.names=FALSE, quote=FALSE)

# Scatterplots

# Plot Weekly Sales by Holiday dummy
ggplot(data = dfTrainMerged) + 
  geom_point(mapping = aes(x = IsHoliday, y = Weekly_Sales)) +
  labs(x = "Holiday", y = "Weekly Sales", title = "Is weekly sales higher on a holiday?")
  geom_smooth(mapping = aes(x = IsHoliday, y = Weekly_Sales))

# Plot Weekly Sales by Type
ggplot(data = dfTrainMerged) + 
  geom_point(mapping = aes(x = Type, y = Weekly_Sales)) +
  labs(x = "Type", y = "Weekly Sales", title = "Is weekly sales higher by type?")
  geom_smooth(mapping = aes(x = Type, y = Weekly_Sales))

# Plot Weekly Sales by Size
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = Size, y = Weekly_Sales)) +
  labs(x = "Size", y = "Weekly Sales", title = "Is weekly sales higher by store size?")

# Plot Weekly Sales by Temperature
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = Temperature, y = Weekly_Sales)) +
  labs(x = "Temperature", y = "Weekly Sales", title = "Is weekly sales higher the hotter it is outside?")

# Plot Weekly Sales by Fuel Price
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = Fuel_Price, y = Weekly_Sales)) +
  labs(x = "Fuel Price", y = "Weekly Sales", title = "What about the relationship with fuel price?")

# Plot Weekly Sales by CPI
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = CPI, y = Weekly_Sales)) +
  labs(x = "CPI", y = "Weekly Sales", title = "Sales are higher at lower CPI (inflation)?")

# Plot Weekly Sales by Unemployment Rate
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = Unemployment, y = Weekly_Sales)) +
  labs(x = "Unemployment", y = "Weekly Sales", title = "Sales are higher at lower unemployment rates?")

# Plot Weekly Sales by Dept
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = Dept, y = Weekly_Sales)) +
  labs(x = "Department", y = "Weekly Sales", title = "Which department performs best?")

# Plot Weekly Sales by Markdown1
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = MarkDown1, y = Weekly_Sales)) +
  labs(x = "Markdown 1", y = "Weekly Sales", title = "Sales during Markdown 1")

# Plot Weekly Sales by Markdown2
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = MarkDown2, y = Weekly_Sales)) +
  labs(x = "Markdown 2", y = "Weekly Sales", title = "Sales during Markdown 2")

# Plot Weekly Sales by Markdown3
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = MarkDown3, y = Weekly_Sales)) +
  labs(x = "Markdown 3", y = "Weekly Sales", title = "Sales during Markdown 3")

# Plot Weekly Sales by Markdown4
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = MarkDown4, y = Weekly_Sales)) +
  labs(x = "Markdown 4", y = "Weekly Sales", title = "Sales during Markdown 4")

# Plot Weekly Sales by Markdown5
ggplot(data = dfTrainMerged) +
  geom_point(mapping = aes(x = MarkDown5, y = Weekly_Sales)) +
  labs(x = "Markdown 5", y = "Weekly Sales", title = "Sales during Markdown 5")

# Bar graph

# Plot Mean Sales per Store
ggplot(dfTrainMerged, aes(x = Store, y = Weekly_Sales)) +
  stat_summary(fun.y=("mean"), geom="bar", fill="steelblue") +
  labs(x = "Weekly Sales", y = "Store", title = "Mean Sales per Store") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks=min(dfTrainMerged$Store):max(dfTrainMerged$Store), expand=c(0,0.1)) 

# Plot Mean Sales per Dept
ggplot(dfTrainMerged, aes(x = Dept, y = Weekly_Sales)) +
  stat_summary(fun.y=("mean"), geom="bar", fill="steelblue") +
  labs(x = "Weekly Sales", y = "Department", title = "Mean Sales per Department") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks=min(dfTrainMerged$Dept):max(dfTrainMerged$Dept), expand=c(0,0.1)) 

# Create dummies
dfTrainMerged$Holiday <- ifelse(dfTrainMerged$IsHoliday == 'TRUE', 1, 0)
dfTrainMerged$NonHoliday <- ifelse(dfTrainMerged$IsHoliday == 'FALSE', 1, 0)
dfTrainMerged$Store4 <- ifelse(dfTrainMerged$Store == '4', 1, 0)
dfTrainMerged$Store14 <- ifelse(dfTrainMerged$Store == '14', 1, 0)
dfTrainMerged$Store20 <- ifelse(dfTrainMerged$Store == '20', 1, 0)
dfTrainMerged$TypeA <- ifelse(dfTrainMerged$Type == 'A', 1, 0)
dfTrainMerged$TypeB <- ifelse(dfTrainMerged$Type == 'B', 1, 0)
dfTrainMerged$TypeC <- ifelse(dfTrainMerged$Type == 'C', 1, 0)
dfTrainMerged$SuperBowl10 <- ifelse(dfTrainMerged$Date == '2010-02-10', 1, 0)
dfTrainMerged$SuperBowl11 <- ifelse(dfTrainMerged$Date == '2011-02-11', 1, 0)
dfTrainMerged$SuperBowl12 <- ifelse(dfTrainMerged$Date == '2012-02-10', 1, 0)
dfTrainMerged$SuperBowl13 <- ifelse(dfTrainMerged$Date == '2013-02-08', 1, 0)
dfTrainMerged$Labor10 <- ifelse(dfTrainMerged$Date == '2010-09-10', 1, 0)
dfTrainMerged$Labor11 <- ifelse(dfTrainMerged$Date == '2011-09-09', 1, 0)
dfTrainMerged$Labor12 <- ifelse(dfTrainMerged$Date == '2012-09-09', 1, 0)
dfTrainMerged$Labor13 <- ifelse(dfTrainMerged$Date == '2013-09-06', 1, 0)
dfTrainMerged$Thanksg10 <- ifelse(dfTrainMerged$Date == '2010-11-26', 1, 0)
dfTrainMerged$Thanksg11 <- ifelse(dfTrainMerged$Date == '2011-11-25', 1, 0)
dfTrainMerged$Thanksg12 <- ifelse(dfTrainMerged$Date == '2012-11-23', 1, 0)
dfTrainMerged$Thanksg13 <- ifelse(dfTrainMerged$Date == '2013-11-29', 1, 0)
dfTrainMerged$Xmas10 <- ifelse(dfTrainMerged$Date == '2010-12-31', 1, 0)
dfTrainMerged$Xmas11 <- ifelse(dfTrainMerged$Date == '2011-12-30', 1, 0)
dfTrainMerged$Xmas12 <- ifelse(dfTrainMerged$Date == '2012-12-28', 1, 0)
dfTrainMerged$Xmas13 <- ifelse(dfTrainMerged$Date == '2013-12-27', 1, 0)

# Clean NAs from MarkDowns
dfTrainMerged$MarkDown1[is.na(dfTrainMerged$MarkDown1)] <- 0
dfTrainMerged$MarkDown2[is.na(dfTrainMerged$MarkDown2)] <- 0
dfTrainMerged$MarkDown3[is.na(dfTrainMerged$MarkDown3)] <- 0
dfTrainMerged$MarkDown4[is.na(dfTrainMerged$MarkDown4)] <- 0
dfTrainMerged$MarkDown5[is.na(dfTrainMerged$MarkDown5)] <- 0

# Clean data (retain numerical only)
dropcor <- c("Date","IsHoliday", "Type", "SuperBowl10", "SuperBowl11", "SuperBowl12", "SuperBowl13", 
             "Labor10", "Labor11", "Labor12", "Labor13",
             "Thanksg10", "Thanksg11", "Thanksg12", "Thanksg13")
dfTrainMerged1 = dfTrainMerged[,!(names(dfTrainMerged) %in% dropcor)]

# Compute a Correlation Matrix
data(dfTrainMerged1)
corr <- round(cor(dfTrainMerged1), 1)
head(corr[, 1:21])

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(dfTrainMerged1)
head(p.mat[, 1:21])

# Visualize the correlation matrix
# method = "square" (default)
ggcorrplot(corr)

# Reordering the correlation matrix
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
# with labels
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", lab = TRUE)

# OLS Regression
lm.sales <- lm(Weekly_Sales~Size+Temperature+Fuel_Price+MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5+CPI+Unemployment+Holiday+Store4+Store14+Store20+TypeA+TypeB, data=dfTrainMerged1)
summary(lm.sales)

# Create beautiful tables
stargazer(lm.sales, type="html", dep.var.labels=c("Weekly Sales"), 
          covariate.labels=c("Store Size","Temperature","Fuel Price",
                             "MarkDown1","MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5", 
                             "CPI", "Unemployment", "Holiday",  
                             "Store4", "Store14", "Store20",
                             "Type A", "Type B"), out="models.htm")

# Export Data Frame to csv
write.csv(dfTrainMerged1,"C:/Documents/Data Science/R Tutorial/Walmart Sales Forecasting/clean.csv", row.names = FALSE)

# Create Month_Yr variable
dfTrainMerged$Month_Yr <- format(as.Date(dfTrainMerged$Date), "%Y-%m")

# Get average sales per month_yr on total population
aggregate <- aggregate( Weekly_Sales ~ Month_Yr, dfTrainMerged , mean )
aggregate1 <- aggregate( Weekly_Sales ~ Date, dfTrainMerged, mean)

# Save dataframe as csv 
write.csv(aggregate, file = "aggregate.csv")
write.csv(aggregate, file = "aggregate1.csv")

# Plot average sales per month and year
ggplot(aggregate1, aes(x=Date)) + 
  geom_line(aes(y=Weekly_Sales)) + 
  labs(title="Time Series of Sales from 2010-2012", 
       subtitle="Is there a cyclical trend in sales?", 
       caption="Source: Walmart", 
       y="Mean Weekly Sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_classic()

# Time series forecasting per store

# Convert dataframe to xts
sales_ts <- xts(aggregate1$Weekly_Sales, as.Date(aggregate1$Date), format='%m/%d/%Y', frequency = 52)
write.csv(sales_ts,"C:/Documents/Data Science/R Tutorial/Walmart Sales Forecasting/sales_ts.csv", row.names = FALSE)

data(sales_ts)
class(sales_ts)
start(sales_ts)
end(sales_ts)
frequency(sales_ts)
summary(sales_ts)
plot(sales_ts) 
abline(reg=lm(sales_ts~time(sales_ts)))
cycle(sales_ts)
boxplot(sales_ts-cycle(sales_ts))

mape <- function(actual,pred) {
  mape <- mean(abs((actual-pred)/actual))*100
  return(mape)
}

# Naive Forecasting Method
naive_mod <- naive(sales_ts, h = 12)
summary(naive_mod)

# Simple Exponential Smoothing
se_model <- ses(sales_ts, h = 12)
summary(se_model)

# Holt's Trend Method
holt_model <- holt(sales_ts, h = 12)
summary(holt_model)

# ARIMA 
arima_model <- auto.arima(sales_ts)
summary(arima_model)

# TBATS
model_tbats <- tbats(sales_ts)
summary(model_tbats)


# Random Forest

# Generate interaction term

# Boxplot (for Averages)
