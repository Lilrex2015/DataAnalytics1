#Data Analytics - 636 - HW4
#Author -  Arif Mitha
# Date 11/18/2022



install.packages("ggplot2" , "dplyr")


library(ggplot2)
library(dplyr)


load('salesData.rda')
load('btma.431.736.f2018.v2.rda')




##############################################
############### Quiz Answers##################
##############################################
##############################################

# Q1 - 0.40

# Q2
#HW.average coefficient estimate
#HW.average standard error,
#textbook.quiz.average coefficient estimate
#textbook.quiz.average standard error


# Q3 - 0.36
# Q4 - 0.86
# Q5 - 0.38
# Q6 - 5.50
# Q7 - 101.25
# Q8 - 5.00
# Q9 - 6.00
# Q10 - Upward sloping line
# Q11 - 5.00
# Q12 - 6.00
# Q13 - 3.90

##############################################
############### QUESTION 1 ###################
##############################################
##############################################


#Q1

# 
# lm(formula, data, subset, weights, na.action,
#    method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#    singular.ok = TRUE, contrasts = NULL, offset, …)

q1Data_LM <- lm(final.raw.score.excluding.bonus~., btma.431.736.f2018) # make the Linear model

#summary() function in R Language is a generic function used to produce result summaries of the results of various model fitting functions.

#Q1a
q1a_Co <-  summary(q1Data_LM)$coefficients[2,1]
print(q1a_Co)

#Answer
#0.40348017

#Q1B

Q1BDF <- btma.431.736.f2018 # tie it to a variable so I dont have to type this stupid name out everytime

Q1B_ScoreTx_Rescale <- Q1BDF$textbook.quiz.average/15

typeof(Q1B_ScoreTx_Rescale)

Q1B_ScoreHw_Rescale <-  Q1BDF$HW.average/20

Q1B_subset <- subset(Q1BDF, select = -c(textbook.quiz.average, HW.average))

Q1B_LM <- lm(final.raw.score.excluding.bonus~., data=Q1B_subset)
print(Q1B_LM)
summary(Q1B_LM)

#Answer
# HW.average coefficient estimate, HW.average standard error, 
# textbook.quiz.average coefficient estimate,textbook.quiz.average standard error


#Q1C


q1CData_LM <- lm(final.raw.score.excluding.bonus~., btma.431.736.f2018)

Q1c_Sum <- summary(q1CData_LM)$coefficients[6,4]
print(Q1c_Sum)

#Answer
#0.3574141

#Q1D

q1D_LM <- lm(final.raw.score.excluding.bonus ~ final.project + post.retake.midterm +
               textbook.quiz.average + HW.average + BANA + post.retake.midterm 
                * BANA , data = btma.431.736.f2018)

Q1D_sum <- summary(q1D_LM)$coefficients[7,4]
print(Q1D_sum)

#Answer
#0.8647938


#Q1E

q1E_sub <- subset(btma.431.736.f2018, select = -c(BANA))

Q1E_LM <-  lm(log(final.raw.score.excluding.bonus) ~ log(final.project) + log(post.retake.midterm) + 
                log(textbook.quiz.average) + log(HW.average), q1E_sub)

Q1E_Sum <-  summary(Q1E_LM)$coefficients[2,1]

print(Q1E_Sum)


#Answer
#0.3806462


##############################################
############### QUESTION 2 ###################
##############################################
##############################################




#Q2


#Q2A
# Q(p) = 50 − 5p. This means that if she sets price at p = 5, then she can expect to sell 25 bottles. If she
# sets price to p = 2, she can expect to sell 40 bottles. Her marginal cost of production (the cost of producing
#       and packaging a single bottle of apple juice) is $1. To the nearest ten cents, what is the optimal price?

Q2_Counter <-  seq(1,9,0.1)
Q2_Sales <- 0 
Q2_Y <- 50
Q2_Y2 <- 45
Q2_Y3 <- 55

for (i in 1:length(Q2_Counter)) {
  
              # Q(p) = 50 − 5p.
  
  answer <-  Q2_Y-(5*Q2_Counter[i])
  Q2_Sales[i] <- answer *(Q2_Counter[i]-1)
  
}

#which max sales
Q2_Sales_Max <- Q2_Counter[which.max(Q2_Sales)]
print(Q2_Sales_Max )
#Answer
# $5.50


Q2_Max <-  max(Q2_Sales)
print(Q2_Max)

#Answer 
#101.25



#2B

for (i in 1:length(Q2_Counter)) {
  
  # Q(p) = 50 − 5p.
  answer <-  Q2_Y2-(5*Q2_Counter[i])
  Q2_Sales[i] <- answer *(Q2_Counter[i]-1)
  
}

#which max sales
Q2_Sales_Max <- Q2_Counter[which.max(Q2_Sales)]
print(Q2_Sales_Max)
#Answer
# 5.00


Q2_Max <-  max(Q2_Sales)
print(Q2_Max)

#Answer 
# 80


for (i in 1:length(Q2_Counter)) {
  
  # Q(p) = 50 − 5p.
  answer <-  Q2_Y3-(5*Q2_Counter[i])
  Q2_Sales[i] <- answer *(Q2_Counter[i]-1)
  
}

#which max sales
Q2_Sales_Max <- Q2_Counter[which.max(Q2_Sales)]
print(Q2_Sales_Max)
#Answer
# 6.00


Q2_Max <-  max(Q2_Sales)
print(Q2_Max)

#Answer 
#125



#2C

Q2C <- seq(40, 60, by = 1)
Q2C_DF <- data.frame(Q2C)
Count2C <- seq( from = 1, to = 9 ,by = 0.1)
for (w in 1:length(Q2C)) {
  Sales <- 0
  for (i in 1:length(Count2C)) {
    Q <- Q2C[w]-(5* Count2C [i])
    Sales[i] <- Q*( Count2C [i]-1)
  }
  Q2C_DF [w,2] <- Count2C [which.max(Sales)] }
Q2C_DF$Q2C <- as.vector(Q2C_DF$Q2C)
Q2C_DF$V2 <- as.vector(Q2C_DF$V2)


ggplot()+geom_line(data=Q2C_DF , aes(y=Q2C, x=V2))
#ggplot(Q2C_DF , aes(Q2C, V2)) + geom_line()
#Answer
#Upward Sloping Line




#Q2D

Q2D_seq <- seq(2,8, by = 0.01)
value1 <- 45
value2 <- 55
Q2D_SEQ_DF  <- data.frame(Q2D_seq)
Q2D_counter <- seq( from = 1, to = 15 ,by = 0.1)
for (w in 1:length(Q2D_seq)) {
  
  Q2D_S <- 0
  Q2D_S2 <- 0
  for (i in 1:length(Q2D_counter)) {
    
    Q <- value1 - (Q2D_seq[w]*Q2D_counter[i])
    Q2 <- value2 - (Q2D_seq[w]*Q2D_counter[i])
    
    Q2D_S[i] <- Q*(Q2D_counter[i]-1)
    Q2D_S2[i] <- Q2*(Q2D_counter[i]-1)
  }
  
 Q2D_SEQ_DF [w,2] <- Q2D_counter[which.max(Q2D_S)]
 Q2D_SEQ_DF [w,3] <- Q2D_counter[which.max(Q2D_S2)]}


Q2D_SEQ_DF $m <- as.vector(Q2D_SEQ_DF $m)
Q2D_SEQ_DF $V2 <- as.vector(Q2D_SEQ_DF $V2)  

ggplot()+ geom_line(data=Q2D_SEQ_DF , aes(Q2D_seq, V2),colour="red",size=1)+
  
          geom_line(data=Q2D_SEQ_DF , aes(Q2D_seq, V3),colour="Blue",size=1)


Q2D_SEQ_DF.45 <- filter(Q2D_SEQ_DF , V2 == 5)
uu <- mean(Q2D_SEQ_DF.45$V2) 
print(uu)
#Answer
#5.00

Q2D_S4 <- 0
for (i in 1:length(Q2D_counter)) {
  
  Q2 <- value2 - (uu*Q2D_counter[i])
  
  Q2D_S4[i] <- Q2*(Q2D_counter[i]-1)
}

oo <- Q2D_counter[which.max(Q2D_S4)]
print(oo)
#Answer
# 6.00





#I am over it. This is so annoying.


#2E

PriceFx <- function(dataIn, COP)
{
  #print(dataIn) #safety check
  COG <- seq(min(dataIn$price), max(dataIn$price), by = 0.01)
  total <- 0
  
  Q2E_LM <- lm(quantity ~price+ I(price^2), data = dataIn)
  
  for (r in 1:length(COG)) 
    {
    gg <- (((COG[r]* summary(Q2E_LM)$coefficients[2,1] + summary(Q2E_LM)$coefficients[1,1] + ((COG[r]^2)
                                                    * summary(Q2E_LM)$coefficients[3,1]))))* (COG[r]-COP)
      
    #OMG, 6 hrs and another ) issue. I HATE THIS IDE.
      total[r] <- gg
      
  } #end for
  
  
  COG[which.max(total)]
  
  
}#End function


hh <- PriceFx(salesData, 1)
print(hh)

#Answer
#3.92 / 3.90

#End date 11/21/2022

