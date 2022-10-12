# Date: 10/10/2022
# Author: Arif Mitha
# BTMA 636 - Assignment 3








#Q1 - Muffins

getwd()
setwd("L:/UofC/Fall 2022/BTMA 636 - Data Analytics 1/Repo/DataAnalytics1/DA1_HW3")


install.packages("dplyr")
install.packages("ggplot2")
install.packages("rvest")

library(rvest)
library(dplyr)




nMuffin <- 1000000
muffin_Cost_1a <- 0.20
muffin_Sell_Price_1a  <- 2.50
muffin_Mean_1a  <- 120
muffin_Std_1a  <- 25
muffin_Stats_1a  <- rnorm(n = nMuffin, mean = muffin_Mean_1a, sd = muffin_Std_1a)

#1a

Muffin_1a_demand <- 120
Muffin_1a_stock <- 100

muffin_Daily_Report <-  function(muffin_demand, muffin_stock, muffin_cost, muffin_price)
{
  
  if(muffin_demand < muffin_stock)
  {
    
    leftOvers <-  muffin_stock - muffin_demand
    Daily_Sales <-  muffin_demand * muffin_price
    Daily_Cost <-  muffin_stock * muffin_cost
    Waste_Cost <-  leftOvers * muffin_cost
    Daily_Profit <-  Daily_Sales - Daily_Cost
    Sales_Report <- paste0("Todays daily stock was ", muffin_stock, " and the demand was ", muffin_demand, 
                           " each muffin was sold for $", muffin_price, " at a cost of $", muffin_cost, " this puts the total cost to $", Daily_Cost,
                           " and total sales to $", Daily_Sales,
                           " this means the profit for today was $", Daily_Profit, " but since supply was more than demand, there were ", leftOvers,
                           " left over, which came to a loss of ", Waste_Cost)
    print(Sales_Report)
  }
  
  if(muffin_demand > muffin_stock)
  {
    Daily_Sales <-  muffin_stock * muffin_price
    Daily_Cost <-  muffin_stock * muffin_cost
    Daily_Profit <-  Daily_Sales - Daily_Cost
    Sales_Report <- paste0("Todays daily stock was ", muffin_stock, " and the demand was ", muffin_demand, 
                           " each muffin was sold for $", muffin_price, " at a cost of $", muffin_cost, " this puts the total cost to $", Daily_Cost,
                           " and total sales to $", Daily_Sales,
                           " this means the profit for today was $", Daily_Profit)
    print(Sales_Report)
  }
  
  if(muffin_demand == muffin_stock)
  {
    Daily_Sales <-  muffin_demand * muffin_price
    Daily_Cost <-  muffin_stock * muffin_cost
    Daily_Profit <-  Daily_Sales - Daily_Cost
    Sales_Report <- paste0("Today was a good day, the demand was perfect to the supply and the daily stock was ", muffin_stock, 
                           " and the demand was ", muffin_demand, 
                           " each muffin was sold for $", muffin_price, " at a cost of $", muffin_cost, " this puts the total cost to $", Daily_Cost,
                           " and total sales to $", Daily_Sales,
                           " this means the profit for today was $", Daily_Profit)
    print(Sales_Report)
  }
  
 
  
  
}
muffin_Daily_Report(Muffin_1a_demand, Muffin_1a_stock, muffin_Cost_1a, muffin_Sell_Price_1a)
muffin_Daily_Report(80,100,0.5,10)
muffin_Daily_Report(250,80,0.8,25)

