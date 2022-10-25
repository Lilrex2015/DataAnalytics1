# Date: 10/10/2022
# Author: Arif Mitha
# BTMA 636 - Assignment 3








#Q1 - Muffins

getwd()
setwd("L:/UofC/Fall 2022/BTMA 636 - Data Analytics 1/Repo/DataAnalytics1/DA1_HW3")


install.packages("dplyr")
install.packages("ggplot2")
install.packages("rvest")
install.packages("gtools")

library(rvest)
library(dplyr)
library(gtools)
library(ggplot2)



nMuffin <- 1
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
    #print(Sales_Report)
    return(Sales_Report)
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
    #print(Sales_Report)
    return(Sales_Report)
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
    #print(Sales_Report)
    return(Sales_Report)
  }
  
 
  
  
}
#muffin_Daily_Report(Muffin_1a_demand, Muffin_1a_stock, muffin_Cost_1a, muffin_Sell_Price_1a)

#1a 
#If the demand tomorrow is 120 and only 100 muffins were stocked, then muffins are understocked.
#What would be the profit if only 100 muffins were stocked


#(muffin_demand, muffin_stock, muffin_cost, muffin_price)

Q1a <- muffin_Daily_Report(120,100,0.20,2.50)
print(Q1a)

#1b
#If the demand is only 80 and 100 muffins were stocked, then muffins were overstocked. What would be the profit in this case?

Q1b <- muffin_Daily_Report(80,100,0.2,2.50)
print(Q1b)


#1c
#Decision variable -> Amount to buy / Stocking level


#1d

#The expected profit changes for different stocking levels. What is the stocking level that maximizes the expected profit

# Revenue = amount_sold * price
# Expenses = purchased * cost_price
# Profit =  Revenue - Expenses


muffinDF <-  data.frame()
N <-  300
muffin_Stats_1d  <- rnorm(n = 1000000, mean = 120, sd = 25)
#quiz 155
for (i  in 1:N )
{

  simulation_demand <-  pmin(muffin_Stats_1d, i)
  profits <- simulation_demand * 2.50 - (i * 0.20)
  
  muffinDF_temp <-  data.frame(Quantity_Stock = i, profit = mean(profits))
  muffinDF <- bind_rows(muffinDF, muffinDF_temp)
  
  
  
}
which_max_profit <- which.max(muffinDF$profit)
print(paste0("1D which max for profit is ", which_max_profit, " muffins")) #<- this might take a second to appear in console


#1e
# At the optimal stocking level, what’s the chance that all the demand for tomorrow will be met?
crit_fract <- pnorm(155, mean = 120, sd = 25, lower.tail = TRUE)

print(crit_fract)


#1f
# Conditional on running out, how much excess demand is there on average if stocking optimally?
muffin_Stats_1f  <- data.frame(demand = rnorm(n = 1000000, mean = 120, sd = 25)) 

over_demand <-  muffin_Stats_1f %>% filter(demand > 155)
over_difference_1f <- data.frame(difference = over_demand - 155)
OD_avg <-  mean(over_difference_1f$demand)

print(OD_avg)
#1g
# Conditional on overstocking, how much excess supply is there on average if stocking optimally?

muffin_Stats_1g  <- data.frame(demand = rnorm(n = 1000000, mean = 120, sd = 25)) 

over_demand_1g <-  muffin_Stats_1g %>% filter(demand < 155)
over_difference_1g <- data.frame(difference = (155 - over_demand_1g))
OD_avg_1g <-  mean(over_difference_1g$demand)

print(OD_avg_1g)


#1h
#Create a plot of your expected profit at the optimal stocking level as a function of the standard deviation. 

temp_sd <-  seq(0,30,by = 5)

plot_1h <-  function(standard_dev)
{
  
  
  muffinDF_1h <-  data.frame()
  N <-  300
  muffin_Stats_1d  <- rnorm(n = 1000000, mean = 120, sd = standard_dev)

  for (i  in 1:N )
  {
    
    simulation_demand <-  pmin(muffin_Stats_1d, i)
    profits <- simulation_demand * 2.50 - (i * 0.20)
    
    muffinDF_temp <-  data.frame(Quantity_Stock = i, profit = mean(profits))
    muffinDF_1h <- bind_rows(muffinDF_1h, muffinDF_temp)
    
    
    
  }
  which_max_profit <- which.max(muffinDF_1h$profit)

  print(which_max_profit)
  
  return(which_max_profit)
  
}#end function

gg <- sapply(temp_sd, plot_1h)
plot(temp_sd, gg)




#1i
#Suppose that the folks running the bakery Brew and Blendz buys the muffins from are celebrating their ten-year anniversary tomorrow by giving 
#Brew and Blendz 100 muffins for free. 
#How many additional muffins on top of the free 100, if any, should Brew and Blendz purchase tomorrow
#(at the regular price)? Note: Brew and Blendz is not giving muffins away for free, only their supplier is.


how_many_to_buy <- which_max_profit - 100
print(paste0(how_many_to_buy, " is required to purchase"))

########################################################################################################
################################## Question 2 ##########################################################
########################################################################################################



#2


# Customer -> if time > 30 min then $3 for every extra 30
# Subscriber -> if time > 45 min then $2.50 per 15 min
#if time is over 24 hrs, considered stolen



url <- "https://s3.amazonaws.com/tripdata/202101-citibike-tripdata.csv.zip"

temp <- tempfile()
download.file(url, temp)
citibike <- read.csv(unz(temp, "202101-citibike-tripdata.csv"),
                     stringsAsFactors = FALSE)
unlink(temp)

citibike.trips <- citibike %>%
  filter(usertype == "Subscriber" | usertype == "Customer") %>% filter(tripduration < 86400 )




#2a

PerSecondCharge <-  function()
 
{
  customer_overageDF <-  data.frame()
  customer_overageDF_2D <-  data.frame()
  
  for (i  in 1:nrow(citibike.trips) )
  {
    role <- tolower(citibike.trips$usertype[i])
    print(role)
    
    

    time <- citibike.trips$tripduration[i]
    
    if(role == 'customer')
    {
  
      
      
      if(time > 1800)
      {
        rate = 3.00
        
        time_calc_c <-  time -  1800
        total_rate <- rate / (30 * 60)
        chargeAmountPerSec <- time_calc_c * total_rate
        
        #2D
        
        
        calc_for_2D_c <-  ((time_calc_c / 60) / 30)
        static_time_2d_c <-  ceiling(calc_for_2D_c)* rate
        
        print(paste0("charge amount $" , chargeAmountPerSec))
        customer_overageDF_temp <-  data.frame(customer_type = citibike.trips$usertype[i], overage_cost = chargeAmountPerSec)
        customer_overageDF <- bind_rows(customer_overageDF, customer_overageDF_temp)
        
        customer_overageDF_temp_2D <-  data.frame(customer_type = citibike.trips$usertype[i], overage_cost = static_time_2d_c)
        customer_overageDF_2D <- bind_rows(customer_overageDF_2D, customer_overageDF_temp_2D)
        
        
        
        
        

      } #end IF
     
      else
     {
       
       print(" ")
       
       
     } #End Else
      

    }  #end IF
    
    
    else if(role == "subscriber")
    {
      
      
      if(time > 2700 )
      {
        rate = 2.50
        time_calc_s =  time - 2700
        total_rate <- as.double(rate / (15 * 60)) #2A
        chargeAmountPerSec <-  total_rate * time_calc_s
        print(chargeAmountPerSec)
        
        #2D
        calc_for_2D_s <-  ((time_calc_s / 60) / 15) 
        static_time_2d_s <-  ceiling(calc_for_2D_s) * rate
        
        
        customer_overageDF_temp <-  data.frame(customer_type = citibike.trips$usertype[i], overage_cost = chargeAmountPerSec)
        customer_overageDF <- bind_rows(customer_overageDF, customer_overageDF_temp)
        
        
        
        customer_overageDF_temp_2D <-  data.frame(customer_type = citibike.trips$usertype[i], overage_cost = static_time_2d_s)
        customer_overageDF_2D <- bind_rows(customer_overageDF_2D, customer_overageDF_temp_2D)
       
      } #end IF
      
   
      else
      {
        
        print(" ")
      }
      
      
    }    #end Else IF
    else
    {
      
      
      return("Person is not a customer or a subscriber, probably best to call the police")
      
    }     #End Else
    
    
  }#End For Loop
  
  print(customer_overageDF) 
  
  # get_average_Sub <-  mean(customer_overageDF$overage_cost[customer_overageDF$customer_type == 'Subscriber'])
  # print(paste0("Subscriber Average 2A: ", get_average_Sub))
  
   #get_average_Cus <-  mean(customer_overageDF$overage_cost[customer_overageDF$customer_type == 'Customer'])
   #print(paste0("Customer Average 2A: ", get_average_Cus))
   
  #2A 
  get_total_mean <-   mean(customer_overageDF$overage_cost)
  print(paste0("2A Total mean ", get_total_mean))
  
  #QA:  4.00 
   
   
   
   
   #2b
   std_overage_2b <-  sd(customer_overageDF$overage_cost)
   print(paste0("2B Standard Deviation: ", std_overage_2b))
   
   #QA: 14.00
  
   #2c
   revenue_of_overage_s <- sum(customer_overageDF$overage_cost[customer_overageDF$customer_type == 'Subscriber'])
   print(paste0("2C Revenue of Subcribers $", revenue_of_overage_s))
   
   #QA: $76000
   
   revenue_of_overage_c <- sum(customer_overageDF$overage_cost[customer_overageDF$customer_type == 'Customer'])
   print(paste0("2C Revenue of Customers $", revenue_of_overage_c))
   
   #QA: $75000
   
   
   #2d
   
   revenue_of_overage_d_c <-  mean(customer_overageDF_2D$overage_cost[customer_overageDF$customer_type == 'Customer'])
   print(paste0("2D Customer Static Charge $", revenue_of_overage_d_c))
   
   revenue_of_overage_d_s <-  mean(customer_overageDF_2D$overage_cost[customer_overageDF$customer_type == 'Subscriber'])
   print(paste0("2D Subscriber Static Charge $", revenue_of_overage_d_s))
   
   revenue_of_overage_d_total <-  mean(customer_overageDF_2D$overage_cost)
   print(paste0("2D Total Static Charge $", revenue_of_overage_d_total))
   
   
   #2e
   std_overage_2e <-  sd(customer_overageDF_2D$overage_cost)
   print(paste0("2E Standard Deviation: ", std_overage_2e))
   
   #2F 
   print(paste0("2F the company should do fixed billing"))
   
   #2g
   
   #Yes, the current fixed increment policy has a lower standard deviation
   
   #2h
   # The average cost for a subscriber is $7.00
   # The average cost for a customer is  $5.09
   
   
   }#End Function

PerSecondCharge()



########################################################################################################
######################################## Question 3 ####################################################
########################################################################################################


#3


# purchase probability  = (1 - (p/10))
# p = price
p <- 8
pp <-  (1 - (p/10))
print(pp)
pop <- 50
#3a
#Given fixed price p what type of dist does the total demand have?

#Binomial n=50 sucess prob = (1-(p/10))

#What is the expected DECREASE


calc_3_sd <-  function(price)
{
  prob_3 <-  1-(price/10)
  pp <- rbinom(1000000,50,prob_3)
  cc <- sd(pp)
  print(cc)
  
  
  }


#3b
price_3b = 2.00

answer_3b <-  1-(2/10)
print(answer_3b)

make_num_cus <-  answer_3b * 50
print(make_num_cus)

#3c
price_3c = 2.00
#what is the std?

r3c <- calc_3_sd(2.00)
print(r3c)
#2.823895

#3d
price_3d = 4.00

answer_3d <-  1-(price_3d/10)
print(answer_3d)

make_num_cus_3d <-  answer_3d * 50
print(make_num_cus_3d)

#3e
price_3e = 4.00


r3e <- calc_3_sd(4.00)
print(r3e)
#3.461699





########################################################################################################
######################################## Question 4 ####################################################
########################################################################################################




#4a
# To calculate marginal cost, divide the change in production costs by the change in quantity


nMuffin_4a <- 1
muffin_Cost_4a <- 0.20
muffin_Sell_Price_4a  <- 2.50
muffin_Mean_4a  <- 120
muffin_Std_4a  <- 25
muffin_Stats_4a  <- rnorm(n = nMuffin, mean = muffin_Mean_4a, sd = muffin_Std_4a)

which_max_profit
marginal_cost <-    ((which_max_profit +1)*muffin_Cost_4a) - (which_max_profit * muffin_Cost_4a)/((which_max_profit +1) - which_max_profit)
print(marginal_cost)

#4b
#marginal rev =  change in rev / change in quantity

marg_rev <-  ((101*2.50) - (100 * 2.50)) / (101 - 100)
print(marg_rev)

#4c

#2.3*x = 0.2(1-x)

#2.5x = 0.2

x = 0.2/2.5

qnorm((1-x), 120, 25)

########################################################################################################
######################################## Question 5 ####################################################
########################################################################################################


my_tickets <-  10000
total_tickets <- 20000000000 
duration <- 7

draw_time <-  (60*60*24*7)/3
ticket_prob <-  my_tickets / total_tickets



#5a
zero_wins <- pbinom(0, draw_time, ticket_prob)
print(zero_wins)

zero_wins_two <- 1-pbinom(0, draw_time, ticket_prob)
print(zero_wins_two)

#5b

x <- seq(10000,200000,by = 10000)

bb <- function(x)
  
{
  draw_time_2 <-  (60*60*24*7)/3
  ticket_prob_2 <- x / 20000000000
  bb <- 1-pbinom(0, draw_time_2, ticket_prob_2)
  
  return(bb)
  
}

hh <- sapply(x, bb)


raffle_data <- data.frame(raffle = x ,  probability = hh )



#answer is 160,000 tickets in the raffle_data_frame


#5c

x5c <-  function(x)
{
  
  draw_time_year <-  (60*60*24*365)/3
  ticket_prob_3 <- x / 20000000000
  ff <- rbinom(10000000,draw_time_year, ticket_prob_3)
  
  return(mean(ff))
  
}

 kk <- sapply(10000, x5c) *250
print(kk)
 # $1314.088


#5d

jk <- seq(100,10000000,by = 100)


jk5D <- function(jk)
  
{
  draw_time_year_2 <-  (60*60*24*365)/3
  ticket_prob_4 <- jk / 20000000000
   #N = rounds * T = prob
  ff <- draw_time_year_2 * ticket_prob_4 * 250 
  
  
  return(ff)
}

h5D <- sapply(jk, jk5D)


raffle_data_2 <- data.frame(raffle_tix = jk ,  amount_won= h5D )

qplot(raffle_tix, amount_won, data=raffle_data_2, geom=c("point", "line"), log="x")



########################################################################################################
################################ Question 5 - Bonus ####################################################
########################################################################################################


total_tix <- 20000000000

my_tix_1M <- 1000000

sim_draws_1M <- 1000000

my_tix_100k <- 100000

sim_draws_100k <- 100000




#1M
win_prob <- my_tix_1M/total_tix

sim_time <- ((86400*365*10)/3)

avg_wait <- win_prob * sim_time

time_btw_wins <-  ((86400*365*10)/(86400)) / avg_wait

print(time_btw_wins)

#100k

win_prob_2 <- my_tix_100k/total_tix

sim_time_2 <- ((86400*365*10)/3)

avg_wait_2 <- win_prob_2 * sim_time_2

time_btw_wins_2 <-  ((86400*365*10)/(86400)) / avg_wait_2

print(time_btw_wins_2)


