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
muffin_Cost <- 0.20
muffin_Sell_Price <- 2.50
muffin_Mean <- 120
muffin_Std <- 25
muffin_Stats <- rnorm(n = nMuffin, mean = muffin_Mean , sd = muffin_Std)

#1a

Muffin_1a_demand <- 120
Muffin_1a_stock <- 100




