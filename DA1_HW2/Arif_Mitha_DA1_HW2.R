# Data Analytics HW2
#Author: Arif Mitha




#Q1

#Notes: One initial attempt which becomes the "Pre-take score" <- S
#If S < P then the highest RETAKE score R is at least P + B then the new grade will be P + B if not then updated grade is R
#If Pre-take score S is at least P then if the highest midterm retake score R is AT LEAST S + B 
    #then the updated grade is S+ B, Otherwise it will be R

ExP <- 30
ExB <- 30
ExS <- 29
ExR <- 60

#then the updated midterm grade will be 60 

ExS2 <- 30
ExR2 <- S + 30

#IF ExR2 > S + 30 then new score is S + 30 else it is R 


P <- 0
B <- 0 
S <- 0 
R <- 0


Grade_Analyzer <- function(S, R, P, B)
{
if(S < P)
{
 
 if(R == P+B)
 {
   new_score <-  P+B
   
   return(new_score)
 }
  
  
} #end IF
  
  
  
}#End Function

