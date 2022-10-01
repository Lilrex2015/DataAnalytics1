# Data Analytics HW2
#Author: Arif Mitha


install.packages("ggplot2")
install.packages('tidyr')

library('ggplot2')
library('tidyr')
load("exam_scores1.rda")
load("exam_scores2.rda")
load("exam_scores3.rda")
load("exam_scores4.rda")
load("exam_scores5.rda")
read.csv("BTMA 636 - 797 (Fall 2022).csv")

EX1DF <-  data.frame(exam_scores1)


#READ THESE INSTRUCTIONS CAREFULLY!!!




#Q1

#Notes: One initial attempt which becomes the "Pre-take score" <- S
#If S < P then the highest RETAKE score R is at least P + B then the new grade will be P + B if not then updated grade is R
#If Pre-take score S is at least P then if the highest midterm retake score R is AT LEAST S + B 
    #then the updated grade is S+ B, Otherwise it will be R

# ExP <- 30
# ExB <- 30
# ExS <- 29
# ExR <- 60

#then the updated midterm grade will be 60 

# ExS2 <- 30
# ExR2 <- S + 30

#IF ExR2 > S + 30 then new score is S + 30 else it is R 

#Question 1: The Grade Analyzer function will take in all the score values and return the proper grade value

P <- 30
B <- 35 
S <- 21 
R <- 80



#This function answers questions 1-3,5,6,7 of Q1
Grade_Analyzer <- function(S, R, P, B)
{
  PB <- P+B
  SB <- S+B
  
if(S < P)
{
  print("The highest possible score you can get is P + B:")
  print(PB)
  print("feelsbadman")
  
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
      return(SB)
      
      
    }  #end IF
      
     else
     {
       
       return(R)
       
        #End Else
     }
    
    if(S > P)
    {
      
      return(S)
      
      #end IF
    }
    
   }  #end IF
  
  
}#End Function

Grade_Analyzer(S,R,P,B)

#########################
#########################
#########################

#This function will show the maximum grade a person can get from their retake attempts. Takes in 3 arguments S P and B
#This answers questions 4,8,9,10 of Q1

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


Grade_Estimator(S, P, B)


#########################
#########################
#########################

#MIDTERM RETAKE FUNCTION





#########################
#########################
#########################


#Q2 - RANDOM STUDENT SELECTOR




student_DF <- data.frame(read.csv("BTMA 636 - 797 (Fall 2022).csv"))
print(student_DF)

student_DF <- tibble::rowid_to_column(student_DF,"Student_ID")

student_DF <- student_DF %>% unite('Full_Name', First.Name:Last.Name, remove = FALSE) 
numbers_used <- 1:nrow(student_DF)

print(numbers_used)






RND_Stu_Sel <- function(N, student_DF)
{
  #N <- 33
  person_selected <-  data.frame(Student_Name = character(0), Student_Id_Number = numeric(0) )
  used_numbers <- vector()
  
  if(N < 4)
  {
    
    print("This class size is too small. Roster must be 4 or more")
    

  }    #end IF
  
  else{
    for (i in 1:N) {
      
      
      if(i <= N)
      {
        
        while (TRUE) {
          
          rng_gg <- sample(numbers_used, 1)
          print(rng_gg)
          
          
          if(!rng_gg %in% used_numbers)
          {
            break
          }#end IF
          
        }
        
        
        
        
        
        used_numbers <- append(used_numbers,rng_gg)
        #print(used_numbers)
        print(paste0("Here is a list of the numbers that have already been used ", used_numbers))
        
        name_student <- student_DF$Full_Name[student_DF$Student_ID == rng_gg]
        id_student <- student_DF$Student_ID[student_DF$Student_ID == rng_gg]
        tempDF <- data.frame(name_student, id_student)
        
        
        person_selected = rbind(person_selected,tempDF)
        
        print(person_selected)
        
        
        
        
      } #end IF
      
      
      
    }#End FOR-Loop
    
    
    print(used_numbers)
    
    
  }#End function
    
  }
  


RND_Stu_Sel(5, student_DF)


#########################
#########################
#########################

#########################
#########################
#########################

#########################
#########################
#########################

#Q3

class_roster <- data.frame(read.csv("BTMA 636 - 797 (Fall 2022).csv"))
print(class_roster)
class_roster <- tibble::rowid_to_column(class_roster,"Student_ID")

class_roster <- class_roster %>% unite('Full_Name', First.Name:Last.Name, remove = FALSE) 
numbers_used_Q3 <- 1:nrow(class_roster)
print(class_roster)
print(numbers_used_Q3)


student_marking <- vector()
students_used <- vector()
NQ3 <- 33 
  
if(N < 4)
  {
  
  print("This roster size is too small. Minimum of 4")
  

  }else
    {
      for (i in 1:NQ3) {
        
        
        if(i <= NQ3)
        {
          
          while (TRUE) {
            
            rng_Q3 <- sample(numbers_used_Q3, 1)
            print(rng_Q3)
            
            
            if(!rng_Q3 %in% student_marking)
            {
              break
            }#end IF
            
          
            } #end while
      
   
          student_marking <- append(student_marking, rng_Q3)
          
          for (n in 1:3) {
            
            students_used
            
          }
          
          
        } #end IF
      
        } #End For
      
    }#end Else


