# Author: Arif Mitha
# Date: 09/16/2022
# Class: Data Analytics 1
# Assignment Number 1


# I AM SO SORRY FOR THE STATE OF THIS CODE. IT WAS NICE AT THE START AND THEN I GOT TIRED AND CARED LESS AND LESS

#--------------------------------

# Portfolio 1: 100% BTC;
# Portfolio 2: 50% XRP and 50% LTC; 
# Portfolio 3: 25% ETH, 25% DASH, 25% PPC, and 25% XLM. 
# The weights (the percentages) are with respect to the prices on December 1, 2017



#update.packages() # was getting weird zoo error when loading quantmod so this updates the packages to the repo.

#install.packages("quantmod")
#install.packages("dplyr")
#install.packages('ggplot2')
#install.packages("xlsx")
#install.packages('lubridate')

library("quantmod")
library("dplyr")
library('ggplot2')
library("xlsx")
library("lubridate")


load("HW1_data2022.rData") # load in the file

# write.xlsx(BTC.charts, "C:/Users/StarKiller/Desktop/Toss/Data_Excel.xlsx", sheetName = "BTC", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# write.xlsx(DASH.charts, "C:/Users/StarKiller/Desktop/Toss/Data_Excel.xlsx", sheetName = "DASH", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# write.xlsx(ETH.charts, "C:/Users/StarKiller/Desktop/Toss/Data_Excel.xlsx", sheetName = "ETH", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# write.xlsx(LTC.charts, "C:/Users/StarKiller/Desktop/Toss/Data_Excel.xlsx", sheetName = "LTC", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# write.xlsx(PPC.charts, "C:/Users/StarKiller/Desktop/Toss/Data_Excel.xlsx", sheetName = "PPC", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# write.xlsx(XLM.charts, "C:/Users/StarKiller/Desktop/Toss/Data_Excel.xlsx", sheetName = "XLM", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# write.xlsx(XRP.charts, "C:/Users/StarKiller/Desktop/Toss/Data_Excel.xlsx", sheetName = "XRP", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# 


#1a Long run ROI



#long run ROI = (Close on Sept 1 2022 - Close Dec 1 2017) / Close Dec 1 2017
#what needs to be done?  Do the ROI calculation across all the tables and find the highest value

#BTC
BTC_Sept <- BTC.charts$close[ which(BTC.charts$date == '2022-09-01')]

print(BTC_Sept)

BTC_Dec <- BTC.charts$close[ which(BTC.charts$date == '2017-12-01')]
print(BTC_Dec)


BTC_ROI_Value <- ((BTC_Sept - BTC_Dec) / BTC_Dec)

#DASH
DASH_Sept <- DASH.charts$close[ which(DASH.charts$date == '2022-09-01')]

print(DASH_Sept)

DASH_Dec <- DASH.charts$close[ which(DASH.charts$date == '2017-12-01')]
print(DASH_Dec)


DASH_ROI_Value <- ((DASH_Sept - DASH_Dec) / DASH_Dec)

#ETH
ETH_Sept <- ETH.charts$close[ which(ETH.charts$date == '2022-09-01')]

print(ETH_Sept)

ETH_Dec <- ETH.charts$close[ which(ETH.charts$date == '2017-12-01')]
print(ETH_Dec)


ETH_ROI_Value <- ((ETH_Sept - ETH_Dec) / ETH_Dec)

#LTC
LTC_Sept <- LTC.charts$close[ which(LTC.charts$date == '2022-09-01')]

print(LTC_Sept)

LTC_Dec <- LTC.charts$close[ which(LTC.charts$date == '2017-12-01')]
print(LTC_Dec)


LTC_ROI_Value <- ((LTC_Sept - LTC_Dec) / LTC_Dec)

#PPC
PPC_Sept <- PPC.charts$close[ which(PPC.charts$date == '2022-09-01')]

print(PPC_Sept)

PPC_Dec <- PPC.charts$close[ which(PPC.charts$date == '2017-12-01')]
print(PPC_Dec)


PPC_ROI_Value <- ((PPC_Sept - PPC_Dec) / PPC_Dec)

#XLM
XLM_Sept <- XLM.charts$close[ which(XLM.charts$date == '2022-09-01')]

print(XLM_Sept)

XLM_Dec <- XLM.charts$close[ which(XLM.charts$date == '2017-12-01')]
print(XLM_Dec)


XLM_ROI_Value <- ((XLM_Sept - XLM_Dec) / XLM_Dec)

#XRP
XRP_Sept <- XRP.charts$close[ which(XRP.charts$date == '2022-09-01')]

print(XRP_Sept)

XRP_Dec <- XRP.charts$close[ which(XRP.charts$date == '2017-12-01')]
print(XRP_Dec)


XRP_ROI_Value <- ((XRP_Sept - XRP_Dec) / XRP_Dec)


#ETH with an ROI 2.39 

#----------------------------------
#this is getting the highest ROI value of the set for Question 1 on the quiz
max(BTC_ROI_Value, DASH_ROI_Value, ETH_ROI_Value, LTC_ROI_Value, PPC_ROI_Value, XLM_ROI_Value, XRP_ROI_Value) 
#ETH with 2.399873
#----------------------------------


#1b
#use the mean() fxn to get the daily return. 
# use only the closing price and store them in a vector. mean(x, na.rm=FALSE)
#x <- c()



BTC_N <- BTC.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N <- nrow(BTC_N)

BTC_todays_price <-  BTC_N$close[2:N]
BTC_yesterdays_price <- BTC_N$close[1 : N - 1]

BTC_daily_return <- ((BTC_todays_price - BTC_yesterdays_price) / BTC_yesterdays_price)
print("BTC")
print(BTC_daily_return)
head(BTC_daily_return, 10)
tail(BTC_daily_return,10)
mean(BTC_daily_return)
sd(BTC_daily_return)


DASH_N <- DASH.charts %>% filter(date > as.Date("2017-11-30")& date < as.Date("2022-09-02"))
N <-nrow(DASH_N)
DASH_todays_price <-  DASH_N$close[2:N]
DASH_yesterdays_price <- DASH_N$close[1 : N - 1]

DASH_daily_return <- ((DASH_todays_price - DASH_yesterdays_price) / DASH_yesterdays_price)
print("DASH")
print(DASH_daily_return)
mean(DASH_daily_return)


ETH_N <- ETH.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02")) 
N <- nrow(ETH_N)

ETH_todays_price <-  ETH_N$close[2:N]
ETH_yesterdays_price <- ETH_N$close[1 : N - 1]

ETH_daily_return <- ((ETH_todays_price - ETH_yesterdays_price) / ETH_yesterdays_price)
print("ETH")
print(ETH_daily_return)
mean(ETH_daily_return)


LTC_N <- LTC.charts %>% filter(date > as.Date("2017-11-30")) 
N <-nrow(LTC_N)


LTC_todays_price <-  LTC_N$close[2:N]
LTC_yesterdays_price <- LTC_N$close[1 : N - 1]

LTC_daily_return <- ((LTC_todays_price - LTC_yesterdays_price) / LTC_yesterdays_price)
print("LTC")
print(LTC_daily_return)
mean(LTC_daily_return)


PPC_N <- PPC.charts %>% filter(date > as.Date("2017-11-30")) 
N <- nrow(PPC_N)

PPC_todays_price <-  PPC_N$close[2:N]
PPC_yesterdays_price <- PPC_N$close[1 : N - 1]

PPC_daily_return <- ((PPC_todays_price - PPC_yesterdays_price) / PPC_yesterdays_price)
print("PPC")
print(PPC_daily_return)
mean(PPC_daily_return)


XLM_N <- XLM.charts %>% filter(date > as.Date("2017-11-30")) 
N <- nrow(XLM_N)

XLM_todays_price <-  XLM_N$close[2:N]
XLM_yesterdays_price <- XLM_N$close[1 : N - 1]

XLM_daily_return <- ((XLM_todays_price - XLM_yesterdays_price) / XLM_yesterdays_price)
print("XLM")
print(XLM_daily_return)
mean(XLM_daily_return)


XRP_N <- XRP.charts %>% filter(date > as.Date("2017-11-30"))
N <- nrow(XRP_N) 

XRP_todays_price <-  XRP_N$close[2:N]
XRP_yesterdays_price <- XRP_N$close[1 : N - 1]

XRP_daily_return <- ((XRP_todays_price - XRP_yesterdays_price) / XRP_yesterdays_price)
print("XRP")
print(XRP_daily_return)
mean(XRP_daily_return)



#1c
#use the sd() fxn 



BTC_N <- BTC.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N <- nrow(BTC_N)

BTC_todays_price <-  BTC_N$close[2:N]
BTC_yesterdays_price <- BTC_N$close[1 : N - 1]

BTC_daily_return <- ((BTC_todays_price - BTC_yesterdays_price) / BTC_yesterdays_price)
print("BTC")
print(BTC_daily_return)
head(BTC_daily_return, 10)
tail(BTC_daily_return,10)
sd(BTC_daily_return)



DASH_N <- DASH.charts %>% filter(date > as.Date("2017-11-30")& date < as.Date("2022-09-02"))
N <-nrow(DASH_N)
DASH_todays_price <-  DASH_N$close[2:N]
DASH_yesterdays_price <- DASH_N$close[1 : N - 1]

DASH_daily_return <- ((DASH_todays_price - DASH_yesterdays_price) / DASH_yesterdays_price)
print("DASH")
print(DASH_daily_return)
sd(DASH_daily_return)


ETH_N <- ETH.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02")) 
N <- nrow(ETH_N)

ETH_todays_price <-  ETH_N$close[2:N]
ETH_yesterdays_price <- ETH_N$close[1 : N - 1]

ETH_daily_return <- ((ETH_todays_price - ETH_yesterdays_price) / ETH_yesterdays_price)
print("ETH")
print(ETH_daily_return)
sd(ETH_daily_return)


LTC_N <- LTC.charts %>% filter(date > as.Date("2017-11-30")) 
N <-nrow(LTC_N)


LTC_todays_price <-  LTC_N$close[2:N]
LTC_yesterdays_price <- LTC_N$close[1 : N - 1]

LTC_daily_return <- ((LTC_todays_price - LTC_yesterdays_price) / LTC_yesterdays_price)
print("LTC")
print(LTC_daily_return)
sd(LTC_daily_return)


PPC_N <- PPC.charts %>% filter(date > as.Date("2017-11-30")) 
N <- nrow(PPC_N)

PPC_todays_price <-  PPC_N$close[2:N]
PPC_yesterdays_price <- PPC_N$close[1 : N - 1]

PPC_daily_return <- ((PPC_todays_price - PPC_yesterdays_price) / PPC_yesterdays_price)
print("PPC")
print(PPC_daily_return)
sd(PPC_daily_return)


XLM_N <- XLM.charts %>% filter(date > as.Date("2017-11-30")) 
N <- nrow(XLM_N)

XLM_todays_price <-  XLM_N$close[2:N]
XLM_yesterdays_price <- XLM_N$close[1 : N - 1]

XLM_daily_return <- ((XLM_todays_price - XLM_yesterdays_price) / XLM_yesterdays_price)
print("XLM")
print(XLM_daily_return)
sd(XLM_daily_return)


XRP_N <- XRP.charts %>% filter(date > as.Date("2017-11-30"))
N <- nrow(XRP_N) 

XRP_todays_price <-  XRP_N$close[2:N]
XRP_yesterdays_price <- XRP_N$close[1 : N - 1]

XRP_daily_return <- ((XRP_todays_price - XRP_yesterdays_price) / XRP_yesterdays_price)
print("XRP")
print(XRP_daily_return)
sd(XRP_daily_return)


#2a
#Portfolio 1
#100% BTC
tail(BTC.charts,20)
Open_P1_Acc_BTC_Value <- (10000 / BTC_Dec)
Close_P1_Acc_BTC_Value <- (Open_P1_Acc_BTC_Value * BTC_Sept)
print((Close_P1_Acc_BTC_Value))
print(BTC_Dec)
print(BTC_Sept)


#Portfolio 2
# $5000 in XRP  

Open_P2_Acc_XRP_Value <- (5000 / XRP_Dec)
Close_P2_Acc_XRP_Value <- (Open_P2_Acc_XRP_Value * XRP_Sept)
print((Close_P2_Acc_XRP_Value))
print(XRP_Dec)
print(XRP_Sept)

#$5000 in LTC

Open_P2_Acc_LTC_Value <- (5000 / LTC_Dec)
Close_P2_Acc_LTC_Value <- (Open_P2_Acc_LTC_Value * LTC_Sept)
print((Close_P2_Acc_LTC_Value))
print(LTC_Dec)
print(LTC_Sept)

Portfolio_2_value <- Close_P2_Acc_LTC_Value + Close_P2_Acc_XRP_Value
print(Portfolio_2_value)



#Portfolio 3
#25% ETH 
Open_P3_Acc_ETH_Value <- (2500 / ETH_Dec)
Close_P3_Acc_ETH_Value <- (Open_P3_Acc_ETH_Value * ETH_Sept)
print((Close_P3_Acc_ETH_Value))
print(ETH_Dec)
print(ETH_Sept)


#25% DASH 
Open_P3_Acc_DASH_Value <- (2500 / DASH_Dec)
Close_P3_Acc_DASH_Value <- (Open_P3_Acc_DASH_Value * DASH_Sept)
print((Close_P3_Acc_DASH_Value))


#25% PPC
Open_P3_Acc_PPC_Value <- (2500 / PPC_Dec)
Close_P3_Acc_PPC_Value <- (Open_P3_Acc_PPC_Value * PPC_Sept)
print((Close_P3_Acc_PPC_Value))
print(PPC_Dec)
print(PPC_Sept)

#25% XLM.
Open_P3_Acc_XLM_Value <- (2500 / XLM_Dec)
Close_P3_Acc_XLM_Value <- (Open_P3_Acc_XLM_Value * XLM_Sept)
print((Close_P3_Acc_XLM_Value))
print(XLM_Dec)
print(XLM_Sept)

Portfolio_3_value <- Close_P3_Acc_XLM_Value + Close_P3_Acc_PPC_Value + Close_P3_Acc_DASH_Value + Close_P3_Acc_ETH_Value
print(Portfolio_3_value)

#2b
#plot the portfolios

#Portfolio 1 - BTC

BTC_dates_2b <- BTC.charts$date[BTC.charts$date > as.Date("2017-11-30")]



BTC_close_2b <- BTC.charts$close[BTC.charts$date > as.Date("2017-11-30")]
print(BTC_close_2b)
length(BTC_close_2b)
BTC_df <- data.frame(date2b = BTC_dates_2b, close2b = BTC_close_2b)
BTC_df

qplot(x=date2b, y=close2b, data=BTC_df)


#Portfolio 2
#Need to make both coins into a single Dataframe bc plot wont go

XRP_date = subset(XRP.charts$date, XRP.charts$date>as.Date("2017-11-30"))
XRP_close = subset(XRP.charts$close, XRP.charts$date>as.Date("2017-11-30"))


XRP_2b <- data.frame(XRP_date_df = XRP_date, XRP_close_df = XRP_close, XRP_Return = ((5000 / XRP_Dec) *XRP_close))
                     
                                   print(XRP_2b)
        
  LTC_date = subset(LTC.charts$date, LTC.charts$date>as.Date("2017-11-30"))
  LTC_close = subset(LTC.charts$close, LTC.charts$date>as.Date("2017-11-30"))
                                   
   LTC_2b <- data.frame(LTC_date_df = LTC_date, LTC_close_df = LTC_close, LTC_Return = ((5000 / LTC_Dec) *LTC_close))
                                   

Portfolio2Merged <- data.frame(XRP_Cash = ((5000 / XRP_Dec) * XRP_2b$XRP_close), XRP_Close = XRP_2b$XRP_close,  XRP_Date = XRP_2b$XRP_date,
                               LTC_Cash = ((5000 / LTC_Dec )* LTC_2b$LTC_close), LTC_Date = LTC_2b$LTC_date,
                               Total_Return = XRP_2b$XRP_Return + LTC_2b$LTC_Return)

qplot(x=XRP_2b$XRP_date, y=Total_Return, data=Portfolio2Merged)


#Portfolio 3

ETH_date = subset(ETH.charts$date, ETH.charts$date>as.Date("2017-11-30"))
ETH_close = subset(ETH.charts$close, ETH.charts$date>as.Date("2017-11-30"))


ETH_2b <- data.frame(ETH_date_df = ETH_date, ETH_close_df = ETH_close, ETH_Return = ((2500 / ETH_Dec) *ETH_close))

print(ETH_2b)

DASH_date = subset(DASH.charts$date, DASH.charts$date>as.Date("2017-11-30"))
DASH_close = subset(DASH.charts$close, DASH.charts$date>as.Date("2017-11-30"))
DASH_Dec2 <- DASH.charts$close[ which(DASH.charts$date == '2017-12-01')]
print(DASH_Dec)

DASH_2b <- data.frame(DASH_date_df = DASH_date, DASH_close_df = DASH_close, 
                      DASH_Return = ((2500 / DASH_Dec2) *DASH_close))

print(DASH_2b)

PPC_date = subset(PPC.charts$date, PPC.charts$date>as.Date("2017-11-30"))
PPC_close = subset(PPC.charts$close, PPC.charts$date>as.Date("2017-11-30"))


PPC_2b <- data.frame(PPC_date_df = PPC_date, PPC_close_df = PPC_close, PPC_Return = ((2500 / PPC_Dec) *PPC_close))

print(PPC_2b)

XLM_date = subset(XLM.charts$date, XLM.charts$date>as.Date("2017-11-30"))
XLM_close = subset(XLM.charts$close, XLM.charts$date>as.Date("2017-11-30"))


XLM_2b <- data.frame(XLM_date_df = XLM_date, XLM_close_df = XLM_close, XLM_Return = ((2500 / XLM_Dec) *XLM_close))

print(XLM_2b)

Portfolio2Merged <- data.frame(ETH_Cash = ((2500 / ETH_Dec) * ETH_2b$ETH_close), ETH_Close = ETH_2b$ETH_close,  ETH_Date = ETH_2b$ETH_date,
                               DASH_Cash = ((2500 / DASH_Dec2 )* DASH_2b$DASH_close), DASH_Date = DASH_2b$DASH_date,
                              PPC_Cash = ((2500 / PPC_Dec) * PPC_2b$PPC_close),
                              PPC_Close = PPC_2b$PPC_close,  PPC_Date = PPC_2b$PPC_date,
                              XLM_Cash = ((2500 / XLM_Dec )* XLM_2b$XLM_close), XLM_Date = XLM_2b$XLM_date,


Total_Return = ETH_2b$ETH_Return + DASH_2b$DASH_Return)



qplot(x=ETH_2b$ETH_date, y=Total_Return, data=Portfolio2Merged)

#2c
# Portfolio 1: 100% BTC;
# Portfolio 2: 50% XRP and 50% LTC; 


#port 2 vs prt 1 Daily return
XRP_Raw_Close <-  subset(XRP.charts$close, XRP.charts$date>as.Date("2017-11-30"))
XRP_Raw_Date <-  subset(XRP.charts$date, XRP.charts$date>as.Date("2017-11-30"))


LTC_Raw_Close <-  subset(LTC.charts$close, LTC.charts$date>as.Date("2017-11-30"))
LTC_Raw_Date <-  subset(LTC.charts$date, LTC.charts$date>as.Date("2017-11-30"))

P2_Mixed <-  (5000/XRP_Dec * XRP_Raw_Close) + (5000/LTC_Dec * LTC_Raw_Close)
print(P2_Mixed)

P2_Mixed_Today <- P2_Mixed[2:N]
print(P2_Mixed_Today)

P2_yesterdays_price <- P2_Mixed[1 : N - 1]
print(P2_yesterdays_price)

P2_Daily_return <- ((P2_Mixed_Today - P2_yesterdays_price)/P2_yesterdays_price)
print(P2_Daily_return)


P2DF <- data.frame(P2 = P2_Daily_return, BTCDRT = BTC_daily_return)


corP2 <- cor(P2DF$P2, P2DF$BTCDRT)
print(corP2)

# 


# Portfolio 3: 25% ETH, 25% DASH, 25% PPC, and 25% XLM. 
ETH_Raw_Close <-  subset(ETH.charts$close, ETH.charts$date>as.Date("2017-11-30"))
ETH_Raw_Date <-  subset(ETH.charts$date, ETH.charts$date>as.Date("2017-11-30"))


DASH_Raw_Close <-  subset(DASH.charts$close, DASH.charts$date>as.Date("2017-11-30"))
DASH_Raw_Date <-  subset(DASH.charts$date, DASH.charts$date>as.Date("2017-11-30"))	

XLM_Raw_Close <-  subset(XLM.charts$close, XLM.charts$date>as.Date("2017-11-30"))
XLM_Raw_Date <-  subset(XLM.charts$date, XLM.charts$date>as.Date("2017-11-30"))


PPC_Raw_Close <-  subset(PPC.charts$close, PPC.charts$date>as.Date("2017-11-30"))
PPC_Raw_Date <-  subset(PPC.charts$date, PPC.charts$date>as.Date("2017-11-30"))

P3_Mixed <-  (2500/ETH_Dec * ETH_Raw_Close) + (2500/PPC_Dec * PPC_Raw_Close) + (2500/XLM_Dec * XLM_Raw_Close) + (2500/DASH_Dec * DASH_Raw_Close)

print(P3_Mixed)

P3_Mixed_Today <- P3_Mixed[2:N]
print(P3_Mixed_Today)

P3_yesterdays_price <- P3_Mixed[1 : N - 1]
print(P3_yesterdays_price)

P3_Daily_return <- ((P3_Mixed_Today - P3_yesterdays_price)/P3_yesterdays_price)
print(P3_Daily_return)


P3DF <- data.frame(P3 = P3_Daily_return, BTCDRT = BTC_daily_return)


corP3 <- cor(P3DF$P3, P3DF$BTCDRT)
print(corP3)

#0.71199


#2d
#Portfolio 1
# Portfolio 1: 100% BTC;
#volatility is SD of daily returns
BTC2d <-  subset(BTC.charts, BTC.charts$date>as.Date("2017-11-30") & BTC.charts$date< as.Date("2020-01-01"))
print(BTC2d)

BTCN2D <- nrow(BTC2d)
print(BTCN2D)

BTC2D_Today <- BTC2d$close[2:BTCN2D]
BTC2D_Yesterday <- BTC2d$close[1:BTCN2D - 1]

BTC2D_DRT <-  ((BTC2D_Today - BTC2D_Yesterday)/ BTC2D_Yesterday)
BTC_Volatility <-  sd(BTC2D_DRT)
print(BTC_Volatility)



#Portfolio 2

# Portfolio 2: 50% XRP and 50% LTC; 
XRP2d <-  subset(XRP.charts, XRP.charts$date>as.Date("2017-11-30") & XRP.charts$date < as.Date("2020-01-01"))
print(XRP2d)



XRPN2D <- nrow(XRP2d)
print(XRPN2D)

XRP2D_Today <- XRP2d$close[2:XRPN2D]
XRP2D_Yesterday <- XRP2d$close[1:XRPN2D - 1]

XRP2D_DRT <-  ((XRP2D_Today - XRP2D_Yesterday)/ XRP2D_Yesterday)
XRP_Volatility <-  sd(XRP2D_DRT)
print(XRP_Volatility)


LTC2d <-  subset(LTC.charts, LTC.charts$date>as.Date("2017-11-30") & LTC.charts$date < as.Date("2020-01-01"))
print(LTC2d)


LTCN2D <- nrow(LTC2d)
print(LTCN2D)

LTC2D_Today <- LTC2d$close[2:LTCN2D]
LTC2D_Yesterday <- LTC2d$close[1:LTCN2D - 1]

LTC2D_DRT <-  ((LTC2D_Today - LTC2D_Yesterday)/ LTC2D_Yesterday)
LTC_Volatility <-  sd(LTC2D_DRT)
print(LTC_Volatility)

P2_2d_N <- XRP.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2020-01-01")) %>% nrow()
print(P2_2d_N)




P2_2d_raw <- 5000/LTC_Dec * LTC2d$close + 5000/XRP_Dec* XRP2d$close
print(P2_2d_raw)

P2_2d_today <- P2_2d_raw[2:P2_2d_N]
P2_2d_yesterday <- P2_2d_raw[1:P2_2d_N - 1]

P2_2d_DRT <- ((P2_2d_today - P2_2d_yesterday) / P2_2d_yesterday)
print(P2_2d_DRT)
P2_2D_Vol <- sd(P2_2d_DRT)
print(P2_2D_Vol)


# Portfolio 3: 25% ETH, 25% DASH, 25% PPC, and 25% XLM. 

PPC2D <-  subset(PPC.charts, PPC.charts$date>as.Date("2017-11-30") & PPC.charts$date < as.Date("2020-01-01"))
print(PPC2D)


head(PPC2D, 1)
tail(PPC2D, 1)


PPCN2D <- nrow(PPC2D)
print(PPCN2D)

PPC2D_Today <- PPC2D$close[2:PPCN2D]
PPC2D_Yesterday <- PPC2D$close[1:PPCN2D - 1]

PPC2D_DRT <-  ((PPC2D_Today - PPC2D_Yesterday)/ PPC2D_Yesterday)
PPC_Volatility <-  sd(PPC2D_DRT)
print(PPC_Volatility)


XLM2d <-  subset(XLM.charts, XLM.charts$date>as.Date("2017-11-30") & XLM.charts$date < as.Date("2020-01-01"))
print(XLM2d)


XLMN2D <- nrow(XLM2d)
print(XLMN2D)

XLM2D_Today <- XLM2d$close[2:XLMN2D]
XLM2D_Yesterday <- XLM2d$close[1:XLMN2D - 1]

XLM2D_DRT <-  ((XLM2D_Today - XLM2D_Yesterday)/ XLM2D_Yesterday)
XLM_Volatility <-  sd(XLM2D_DRT)
print(XLM_Volatility)
ETH2d <-  subset(ETH.charts, ETH.charts$date>as.Date("2017-11-30") & ETH.charts$date < as.Date("2020-01-01"))
print(ETH2d)


head(ETH2d, 1)
tail(ETH2d, 1)


ETHN2D <- nrow(ETH2d)
print(ETHN2D)

ETH2D_Today <- ETH2d$close[2:ETHN2D]
ETH2D_Yesterday <- ETH2d$close[1:ETHN2D - 1]

ETH2D_DRT <-  ((ETH2D_Today - ETH2D_Yesterday)/ ETH2D_Yesterday)
ETH_Volatility <-  sd(ETH2D_DRT)
print(ETH_Volatility)


DASH2d <-  subset(DASH.charts, DASH.charts$date>as.Date("2017-11-30") & DASH.charts$date < as.Date("2020-01-01"))
print(DASH2d)


DASHN2D <- nrow(DASH2d)
print(DASHN2D)

DASH2D_Today <- DASH2d$close[2:DASHN2D]
DASH2D_Yesterday <- DASH2d$close[1:DASHN2D - 1]

DASH2D_DRT <-  ((DASH2D_Today - DASH2D_Yesterday)/ DASH2D_Yesterday)
DASH_Volatility <-  sd(DASH2D_DRT)
print(DASH_Volatility)

P3_2d_N <- ETH.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2020-01-01")) %>% nrow()
print(P3_2d_N)



P3_2d_raw <- (2500/ DASH_Dec * DASH2d$close + 2500/ETH_Dec * ETH2d$close + 2500/PPC_Dec * PPC2D$close + 2500/XLM_Dec * XLM2d$close )
print(P3_2d_raw)

P3_2d_today <- P3_2d_raw[2:P3_2d_N]

P3_2d_yesterday <- P3_2d_raw[1:P3_2d_N - 1]

P3_2d_DRT <- ((P3_2d_today - P3_2d_yesterday) / P3_2d_yesterday)
print(P3_2d_DRT)
P3_2D_Vol <- sd(P3_2d_DRT)
print(P3_2D_Vol)

#0.0525


---------------------------------#POST COVID-----------------------------------
#####
BTC2d <-  subset(BTC.charts, BTC.charts$date>as.Date("2019-12-31"))
print(BTC2d)

BTCN2D <- nrow(BTC2d)
print(BTCN2D)

BTC2D_Today <- BTC2d$close[2:BTCN2D]
BTC2D_Yesterday <- BTC2d$close[1:BTCN2D - 1]

BTC2D_DRT <-  ((BTC2D_Today - BTC2D_Yesterday)/ BTC2D_Yesterday)
BTC_Volatility <-  sd(BTC2D_DRT)
print(BTC_Volatility)


#Portfolio 2

# Portfolio 2: 50% XRP and 50% LTC; 
XRP2d <-  subset(XRP.charts, XRP.charts$date>as.Date("2019-12-31"))
print(XRP2d)



XRPN2D <- nrow(XRP2d)
print(XRPN2D)

XRP2D_Today <- XRP2d$close[2:XRPN2D]
XRP2D_Yesterday <- XRP2d$close[1:XRPN2D - 1]

XRP2D_DRT <-  ((XRP2D_Today - XRP2D_Yesterday)/ XRP2D_Yesterday)
XRP_Volatility <-  sd(XRP2D_DRT)
print(XRP_Volatility)


LTC2d <-  subset(LTC.charts, LTC.charts$date>as.Date("2019-12-31"))
print(LTC2d)


LTCN2D <- nrow(LTC2d)
print(LTCN2D)

LTC2D_Today <- LTC2d$close[2:LTCN2D]
LTC2D_Yesterday <- LTC2d$close[1:LTCN2D - 1]

LTC2D_DRT <-  ((LTC2D_Today - LTC2D_Yesterday)/ LTC2D_Yesterday)
LTC_Volatility <-  sd(LTC2D_DRT)
print(LTC_Volatility)

P2_2d_N <- XRP.charts %>% filter(date > as.Date("2019-12-31")) %>% nrow()
print(P2_2d_N)




P2_2d_raw <- 5000/LTC_Dec * LTC2d$close + 5000/XRP_Dec* XRP2d$close
print(P2_2d_raw)

P2_2d_today <- P2_2d_raw[2:P2_2d_N]
P2_2d_yesterday <- P2_2d_raw[1:P2_2d_N - 1]

P2_2d_DRT <- ((P2_2d_today - P2_2d_yesterday) / P2_2d_yesterday)
print(P2_2d_DRT)
P2_2D_Vol <- sd(P2_2d_DRT)
print(P2_2D_Vol)


# Portfolio 3: 25% ETH, 25% DASH, 25% PPC, and 25% XLM. 

PPC2D <-  subset(PPC.charts, PPC.charts$date>as.Date("2019-12-31"))
print(PPC2D)


head(PPC2D, 1)
tail(PPC2D, 1)


PPCN2D <- nrow(PPC2D)
print(PPCN2D)

PPC2D_Today <- PPC2D$close[2:PPCN2D]
PPC2D_Yesterday <- PPC2D$close[1:PPCN2D - 1]

PPC2D_DRT <-  ((PPC2D_Today - PPC2D_Yesterday)/ PPC2D_Yesterday)
PPC_Volatility <-  sd(PPC2D_DRT)
print(PPC_Volatility)


XLM2d <-  subset(XLM.charts, XLM.charts$date>as.Date("2019-12-31"))
print(XLM2d)


XLMN2D <- nrow(XLM2d)
print(XLMN2D)

XLM2D_Today <- XLM2d$close[2:XLMN2D]
XLM2D_Yesterday <- XLM2d$close[1:XLMN2D - 1]

XLM2D_DRT <-  ((XLM2D_Today - XLM2D_Yesterday)/ XLM2D_Yesterday)
XLM_Volatility <-  sd(XLM2D_DRT)
print(XLM_Volatility)
ETH2d <-  subset(ETH.charts, ETH.charts$date>as.Date("2019-12-31"))
print(ETH2d)


head(ETH2d, 1)
tail(ETH2d, 1)


ETHN2D <- nrow(ETH2d)
print(ETHN2D)

ETH2D_Today <- ETH2d$close[2:ETHN2D]
ETH2D_Yesterday <- ETH2d$close[1:ETHN2D - 1]

ETH2D_DRT <-  ((ETH2D_Today - ETH2D_Yesterday)/ ETH2D_Yesterday)
ETH_Volatility <-  sd(ETH2D_DRT)
print(ETH_Volatility)


DASH2d <-  subset(DASH.charts, DASH.charts$date>as.Date("2019-12-31"))
print(DASH2d)


DASHN2D <- nrow(DASH2d)
print(DASHN2D)

DASH2D_Today <- DASH2d$close[2:DASHN2D]
DASH2D_Yesterday <- DASH2d$close[1:DASHN2D - 1]

DASH2D_DRT <-  ((DASH2D_Today - DASH2D_Yesterday)/ DASH2D_Yesterday)
DASH_Volatility <-  sd(DASH2D_DRT)
print(DASH_Volatility)

P3_2d_N <- ETH.charts %>% filter(date > as.Date("2019-12-31")) %>% nrow()
print(P3_2d_N)



P3_2d_raw <- (2500/ DASH_Dec * DASH2d$close + 2500/ETH_Dec * ETH2d$close + 2500/PPC_Dec * PPC2D$close + 2500/XLM_Dec * XLM2d$close )
print(P3_2d_raw)

P3_2d_today <- P3_2d_raw[2:P3_2d_N]

P3_2d_yesterday <- P3_2d_raw[1:P3_2d_N - 1]

P3_2d_DRT <- ((P3_2d_today - P3_2d_yesterday) / P3_2d_yesterday)
print(P3_2d_DRT)
P3_2D_Vol <- sd(P3_2d_DRT)
print(P3_2D_Vol)


#--------------------------------------------------



#2e
#get above and and below 0 use filter function


BTC_N_2e <- BTC.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N_2e <- nrow(BTC_N_2e)


BTC_todays_price_2e <-  BTC_N_2e$close[2:N_2e]
BTC_yesterdays_price_2e <- BTC_N_2e$close[1 : N_2e - 1]

BTC_daily_return_2e <- data.frame((BTC_todays_price_2e - BTC_yesterdays_price_2e) / BTC_yesterdays_price_2e)




AA <- c(NA, (BTC_todays_price_2e - BTC_yesterdays_price_2e) / BTC_yesterdays_price_2e)
BB <- c(NA,P2_Daily_return)


P2_2e <- data.frame(BTC_N_2e, BTC_DRT_2e = AA, P2_DRT_2e = BB)
P2_2e_Positive <-  filter(P2_2e, P2_2e$BTC_DRT_2e >=0)
P2_2e_Negative <-  filter(P2_2e, P2_2e$BTC_DRT_2e <=0)

mean(P2_2e_Positive$P2_DRT_2e)
mean(P2_2e_Negative$P2_DRT_2e)

CC <- c(NA, P3_Daily_return)

P3_2e <- data.frame(BTC_N_2e, BTC_DRT_2e = AA, P3_DRT_2e = CC)
P3_2e_Positive <-  filter(P3_2e, P3_2e$BTC_DRT_2e >=0)
P3_2e_Negative <-  filter(P3_2e, P3_2e$BTC_DRT_2e <=0)

mean(P3_2e_Positive$P3_DRT_2e)
mean(P3_2e_Negative$P3_DRT_2e)


#3

#Portfolio 1
BTC_3N <- BTC.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N3 <- nrow(BTC_3N)

BTC_Avg <- data.frame(Average = (BTC_3N$high + BTC_3N$low) /2)
BTC_Avg_Today <-   BTC_Avg$Average[2:N3]
BTC_Avg_yesterday <- BTC_Avg$Average[1 : N3 - 1]

BTC_daily_return_3 <- ((BTC_Avg_Today - BTC_Avg_yesterday) / BTC_Avg_yesterday)

mean(BTC_daily_return_3)



#Portfolio 2

XRP_3N <- XRP.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N3 <- nrow(XRP_3N)

XRP_Avg <- data.frame(Average = (XRP_3N$high + XRP_3N$low) /2)
XRP_Avg_Today <-   XRP_Avg$Average[2:N3]
XRP_Avg_yesterday <- XRP_Avg$Average[1 : N3 - 1]

XRP_daily_return_3 <- ((XRP_Avg_Today - XRP_Avg_yesterday) / XRP_Avg_yesterday)

mean(XRP_daily_return_3)

LTC_3N <- LTC.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N3 <- nrow(LTC_3N)

LTC_Avg <- data.frame(Average = (LTC_3N$high + LTC_3N$low) /2)
LTC_Avg_Today <-   LTC_Avg$Average[2:N3]
LTC_Avg_yesterday <- LTC_Avg$Average[1 : N3 - 1]

LTC_daily_return_3 <- ((LTC_Avg_Today - LTC_Avg_yesterday) / LTC_Avg_yesterday)

mean(LTC_daily_return_3)

P2_3_Avg <- data.frame(Average = XRP_Avg + LTC_Avg)
P2_3_Today <-   P2_3_Avg$Average[2:N3]
P2_3_yesterday <- P2_3_Avg$Average[1 : N3 - 1]

P2_3_ROI <- ((P2_3_Today - P2_3_yesterday) / P2_3_yesterday)
mean(P2_3_ROI)



#Portfolio 3

ETH_3N <- ETH.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N3 <- nrow(ETH_3N)

ETH_Avg <- data.frame(Average = (ETH_3N$high - ETH_3N$low) /2)
ETH_Avg_Today <-   ETH_Avg$Average[2:N3]
ETH_Avg_yesterday <- ETH_Avg$Average[1 : N3 - 1]

ETH_daily_return_3 <- ((ETH_Avg_Today - ETH_Avg_yesterday) / ETH_Avg_yesterday)

mean(ETH_daily_return_3)


DASH_3N <- DASH.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N3 <- nrow(DASH_3N)

DASH_Avg <- data.frame(Average = (DASH_3N$high - DASH_3N$low) /2)
DASH_Avg_Today <-   DASH_Avg$Average[2:N3]
DASH_Avg_yesterday <- DASH_Avg$Average[1 : N3 - 1]

DASH_daily_return_3 <- ((DASH_Avg_Today - DASH_Avg_yesterday) / DASH_Avg_yesterday)

mean(DASH_daily_return_3)


PPC_3N <- PPC.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N3 <- nrow(PPC_3N)

PPC_Avg <- data.frame(Average = (PPC_3N$high - PPC_3N$low) /2)
PPC_Avg_Today <-   PPC_Avg$Average[2:N3]
PPC_Avg_yesterday <- PPC_Avg$Average[1 : N3 - 1]

PPC_daily_return_3 <- ((PPC_Avg_Today - PPC_Avg_yesterday) / PPC_Avg_yesterday)

mean(PPC_daily_return_3)

XLM_3N <- XLM.charts %>% filter(date > as.Date("2017-11-30") & date < as.Date("2022-09-02"))
N3 <- nrow(XLM_3N)

XLM_Avg <- data.frame(Average = (XLM_3N$high - XLM_3N$low) /2)
XLM_Avg_Today <-   XLM_Avg$Average[2:N3]
XLM_Avg_yesterday <- XLM_Avg$Average[1 : N3 - 1]

XLM_daily_return_3 <- ((XLM_Avg_Today - XLM_Avg_yesterday) / XLM_Avg_yesterday)

mean(XLM_daily_return_3)


P3_3_Avg <- data.frame(Average = ETH_Avg + DASH_Avg + PPC_Avg + XLM_Avg)
P3_3_Today <-   P3_3_Avg$Average[2:N3]
P3_3_yesterday <- P3_3_Avg$Average[1 : N3 - 1]

P3_3_ROI <- ((P3_3_Today - P3_3_yesterday) / P3_3_yesterday)
mean(P3_3_ROI)

#It is Monday 09/19/2022, the comments are lacking bc I am just over this assignment.