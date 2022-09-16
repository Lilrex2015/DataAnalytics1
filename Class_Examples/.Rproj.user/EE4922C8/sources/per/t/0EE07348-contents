example.of.list$example.item
example.of.list[[1]]
example.of.list[[2]]
example.of.list[[3]]
example.of.list$example.item[1,2]
example.of.list[['example.item']]

# getwd() Get working Directory 
# setwd() Set Working Directory

# save(example, file = 'example.rda)

# fix(example) Dont use this, not greatest idea, it is messy and not tracked

#load(file = 'example.rda') The file must be in the working directory

# rm(example) removes

# ls() all the objects in a collection
# rm(list-ls())



#for-loops

for (i in 1:3)
{
  print(i)
}


# vectorizing allows for avoiding nested looping

1:10
2*(1:10)



z.vec <- numeric(0)
z<- 1
for(i in 1:5)
{
  
  z <- 2*z
  z.vec[i]
}

print(z.vec)

#install.packages('ggplot2')
library(ggplot2)
qplot(x = (1:100), y=(1:100)^2, xlab='x', ylab='y')



#Finance Analytics

#install.packages('quantmod') #Lets you download data from stock market
library(quantmod)

# ??packagename gets you the help file

getSymbols(Symbols = 'TSLA', src = 'yahoo', from="2022-09-01")

#env = null just makes it not an Object

index(TSLA)


TSLA.data <- data.frame(ticker.date = as.Date(index(TSLA)),Closing.price = as.numeric(TSLA$TSLA.Close))

N <- nrows(TSLA.data)



todays_price <- TSLA.data$closing.price[2:N]
yesterdays_price <- TSLA.data$closing.price[1:N-1]


daily_return <- ((todays_price - yesterdays_price)/yesterdays_price)

