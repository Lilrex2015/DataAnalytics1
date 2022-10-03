
# install.packages("ggplot2")
# install.packages('tidyr')
# install.packages('dplyer')

library('ggplot2')
library('tidyr')
library('dplyr')

load("exam_scores1.rda")
load("exam_scores2.rda")
load("exam_scores3.rda")
load("exam_scores4.rda")
load("exam_scores5.rda")
load("HW2Q4.Rdata")
read.csv("BTMA 636 - 797 (Fall 2022).csv")

EX1DF <-  data.frame(exam_scores1)
EX2DF <-  data.frame(exam_scores2)
EX3DF <-  data.frame(exam_scores3)
EX4DF <-  data.frame(exam_scores4)
EX5DF <-  data.frame(exam_scores5)



average_of_exam <-  mean(EX5DF$exam_scores5)
print(average_of_exam)

rows_of_scores = nrow(EX1DF) #all are 30 so it doesn't matter which exam is used.

counter = 0;
master_Data_Frame <-  data.frame(MeanEx = numeric(0), StdDiv = numeric(0), P_value = numeric(0), B_value = numeric(0), SB_Total = numeric(0))


# P and B need to be multiples of 2 easiest way is to just increments by 2, or could % 2 from top down but too much work
#Going on assumption of P and B are both native 30 since all other questions used that number. Nothing else was specified
#src https://stat.ethz.ch/pipermail/r-help/2008-July/168753.html
#exam scores = S, read about part for S+B




exam_body <- EX4DF
exam_body$P <- NA
exam_body$B <- NA
exam_body$Total <- NA


#This function answers questions 1-3,5,6,7 of Q1
Grade_Analyzer <- function(S, R, P, B)
{
  PB <- P+B
  SB <- S+B
  
  if(S < P)
  {
    print("The highest possible score you can get is P + B:")
    print(PB)
    
    
    if(R == PB)
    {
      new_score <-  PB
      print("score 1")
      return(new_score)
    }
    else if(R < PB)
    {
      
      print("score 2")
      return(R)
      
    } #END ELSE IF
    
    else if(R > PB)
    {
      
      print("score 3")
      return(PB)
      
    }#end Else IF
    
    
    else
    {
      print("score 4")
      
      return(R)
      
    }#End Else
    
    
  } #end IF
  if(S == P)
  {
    if(R == SB)
    {
      print("score 5")
      return(SB)
      
      
    }  #end IF
    
    else
    {
      print("score 6")
      return(R)
      
      #End Else
    }
    
    if(S > P)
    {
      
      print("score 7") 
      return(S)
      
      #end IF
    }
    
  }  #end IF
  
  
}#End Function


Grade_Estimator <- function(S,P,B)
{
  max_score <- P + B
  
  if(S < P )
    
  {
    return(max_score)
    
    
  }    #end IF
  
  else if(S == P)
  {
    SB_max_score <- S+B
    
    return(SB_max_score)
    
  }#end Else IF
  
  
  
  
  
}#End Function




#Q1B

#check the mean score is greater than 75 and less than 70
#P and B assume 30 each from examples

Q1B_DF <- data.frame()
pb_DF <-  data.frame(PValue = numeric(), BValue = numeric(),MeanScore = numeric(), StdScore = numeric())

if(mean(EX4DF$exam_scores4) < 75)
{
  
  for (pChecker  in 1:30 )
  {
    #both p and b need to be multiples of 2
    pValue = pChecker *2 
    
    for (bChecker  in 1:30 )
    {
      
      bValue <- bChecker *2
      
      if(pValue + bValue < 60)
      {
        Q1B_DF_1 <- data.frame(Col1 = Grade_Estimator(bChecker, pValue, bValue))   
        Q1B_DF <- bind_rows(Q1B_DF, Q1B_DF_1)
        
        #end IF
      }
      if(Q1B_DF$Col1[i] > 70 & Q1B_DF$Col1[i] < 75)
      {
        pb_DF <- bind_rows(PValue = pValue, BValue = bValue, MeanScore = mean(Q1B_DF$Col1), StdScore = std(Q1B_DF$Col1))



      }#end IF
    }
    
  }
  
 
} #end IF
