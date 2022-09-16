# Author: Arif Mitha
# Date: 09/16/2022
# Class: Data Analytics 1
# Assignment Number 1
#--------------------------------

# Portfolio 1: 100% BTC;
# Portfolio 2: 50% XRP and 50% LTC; 
# Portfolio 3: 25% ETH, 25% DASH, 25% PPC, and 25% XLM. 
# The weights (the percentages) are with respect to the prices on December 1, 2017

load("HW1_data2022.rData") # load in the file

update.packages() # was getting weird zoo error when loading quantmod so this updates the packages to the repo.

install.packages("quantmod")
install.packages("dplyr")
library("quantmod")
library("dplyr")

#1a Long run ROI



#long run ROI = (Close on Sept 1 2022 - Close Dec 1 2017) / Close Dec 1 2017
#what needs to be done?  Do the ROI calculation across all the tables and find the highest value

#BTC
BTC_Sept <- filter_if(BTC.charts, BTC.charts$date == "2022-09-01")
#BTC_Dec <- df[BTC.charts$date == '2017-12-01']
print(BTC_Sept)
#print(BTC_Dec)


#Dash
#ETH
#LTC
#PPC
#XLM
#XRP
#1b
#use the mean() fxn to get the daily return. 
# redo the loop of all tables and do high - low and store them in a vector. mean(x, na.rm=FALSE)
#x <- c()


#1c
#use the sd() fxn 

#2a
#2b
#2c
#2d
#2e
#3