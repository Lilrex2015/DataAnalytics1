# Data Analytics HW2
#Author: Arif Mitha

getwd()
setwd("/DA1_HW2")

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
rosterFile <- read.csv("BTMA 636 - 797 (Fall 2022).csv")


EX1DF <-  data.frame(exam_scores1)
EX2DF <-  data.frame(exam_scores2)
EX3DF <-  data.frame(exam_scores3)
EX4DF <-  data.frame(exam_scores4)
EX5DF <-  data.frame(exam_scores5)

###############################################################################################################################################################################
######################################################################### READ THESE INSTRUCTIONS CAREFULLY!!! ################################################################
###############################################################################################################################################################################
#Q1 functions
#Q1 has 2 functions, Grade_Analyzer, and Grade_Estimator
#Grade_Analyzer is a function that takes in 4 agruments (S,R,P,B) where S is the Pre-Take score, R is the retake, P and B are as noted.
#This function will return the maximum grade a person can obtain.

#Grade_Analyzer(S,R,P,B) 

#Grade_Estimator is a function that takes in 3 arguments, S,P, and B. Where S is the Pre-take Score, and P and B are as described.
#This function will determine the highest possible Retake score a person can get.

#Grade_Estimator(S, P, B)

###############################################################################################################################################################################
#Q1B *Not Working, Code is commented out
###############################################################################################################################################################################

#Q2

#This function is called RND_Stu_Sel(N, data_frame) and it is built to randomly select students for an instructor.
#It takes 2 arguments, an N value which is the number of students the instructor wants to pick, and a data frame which is a roster list of people to pick from.


#RND_Stu_Sel(5, student_DF)

###############################################################################################################################################################################

#Q3

#This function will choose from a class roster, 1 person to be the marker and 3 projects for them to mark. 
#This function takes in 1 argument which is a roster file in csv format. This change can be made on line 21. 
#The output for this function is a data frame called fullName_df. 

#Q31to3Marker(rosterFile)

###############################################################################################################################################################################

#Q4 

#Q4 has 3 functions. The first one is Q4atoc(player_guess). The player_guess is the number input for comparing against the instructor guess. 
#The 3 function examples below are test examples.

#Q4atoc(player_guess_30)
#Q4atoc(player_guess_450)
#Q4atoc(player_guess_950)

#Q4D
#This function measures the expected loss. It takes in one argument which is a range of numbers 1:1001

#Q4D(N4D)

#Q4E
#This is not a function, it takes the output from 4D and runs it back in through the same 4D function.

#Q4F

#This function measures the best number to minimize the expected loss.  It takes in one argument which is the file of numbers.

#Q4F(HwQ4File)


###############################################################################################################################################################################
##################################################################### QUIZ ANSWERS ##########################################################################################
###############################################################################################################################################################################

#1 -> 50
#2 -> 60
#3 -> 60
#4 -> 60
#5 -> 67
# 6-> 78
# 7-> 84
# 8-> 84
# 9-> 60
# 10-> 116
# 11-> 26
# 12-> 34
# 13-> 30
# 14-> 30
# 15-> 38
# 16-> 22
# 17-> 42
# 18-> 18
# 19-> 22
# 20-> 38
# 21-> 2
# 22-> yes
# 23-> yes
# 24-> yes
# 25-> no
# 26-> yes
# 27-> 1
# 28-> no
# 29-> No
# 30-> No
# 31-> 305,000
# 32-> 285,000
# 33-> 85,000
# 34-> 500
# 35-> 85,000
# 36-> 12,390


###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################



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


Grade_Analyzer(S,R,P,B)
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


 P <- 22
 B <- 30 
 S <- 54 
 R <- 67




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





#########################
#########################
#########################

Grade_Estimator(S, P, B)


#1B
#MIDTERM RETAKE FUNCTION

# EX1DF
# EX2DF
# EX3DF
# EX4DF
# EX5DF


#Q1B - This I could not get working. I am over it. It is 10pm 10/03/2022, I am tired. 

#check the mean score is greater than 75 and less than 70
#P and B assume 30 each from examples

# Q1B_DF <- data.frame()
# pb_DF <-  data.frame(PValue = numeric(), BValue = numeric(),MeanScore = numeric(), StdScore = numeric())
# 
# if(mean(EX4DF$exam_scores4) < 75)
# {
#   
#   for (pChecker  in 1:30 )
#   {
#     #both p and b need to be multiples of 2
#     pValue = pChecker *2 
#     
#     for (bChecker  in 1:30 )
#     {
#       
#       bValue <- bChecker *2
#       
#       if(pValue + bValue < 60)
#       {
#         
#         #dont need all grades just the average
#         Q1B_DF_1 <- data.frame(Col1 = Grade_Estimator(bChecker, pValue, bValue))   
#         Q1B_DF <- bind_rows(Q1B_DF, Q1B_DF_1)
#         
#         #end IF
#         pb_DF <- bind_rows(PValue = pValue, BValue = bValue, MeanScore = mean(Q1B_DF$Col1), StdScore = std(Q1B_DF$Col1))
#       }
#       if(Q1B_DF$Col1[i] > 70 & Q1B_DF$Col1[i] < 75)
#       {
#         
#         
#         
#         
#       }#end IF
#     }
#     
#   }
#   
#   
# } #end IF


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
        print(paste0("Here is a list of the numbers that have been used ", used_numbers))
        
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


Q31to3Marker <- function(rosterFile)
{
  #class_roster <- data.frame(rosterFile)
  class_roster <- data.frame(read.csv("BTMA 636 - 797 (Fall 2022).csv"))
  print(class_roster)
  class_roster <- tibble::rowid_to_column(class_roster,"Student_ID")
  
  class_roster <- class_roster %>% unite('Full_Name', First.Name:Last.Name, remove = FALSE) 
  numbers_used_Q3 <- 1:nrow(class_roster)
  print(class_roster)
  print(numbers_used_Q3)
  fullName_df <- data.frame()
  
  
  Projects_Assignments = data.frame(Marker_ID = 1:nrow(class_roster),
                                    
                                    Project_Student_ID_1 = c(nrow(class_roster), 1:(nrow(class_roster)-1)),
                                    
                                    Project_Student_ID_2 = c((nrow(class_roster)-1):nrow(class_roster), 1:(nrow(class_roster)-2)),
                                    
                                    Project_Student_ID_3 = c((nrow(class_roster)-2):nrow(class_roster), 1:(nrow(class_roster)-3)))
  
  for (i  in 1:nrow(Projects_Assignments))
  {
    
    fullName_df_temp <- data.frame(Marker_Name = class_roster$Full_Name[Projects_Assignments$Marker_ID[i]], Project_Name_1 = class_roster$Full_Name[Projects_Assignments$Project_Student_ID_1[i]],Project_Name_2 = class_roster$Full_Name[Projects_Assignments$Project_Student_ID_2[i]], Project_Name_3 = class_roster$Full_Name[Projects_Assignments$Project_Student_ID_3[i]])
    fullName_df <- bind_rows(fullName_df, fullName_df_temp)
    
  }
  
  print(fullName_df)
  
}

Q31to3Marker(rosterFile)




#########################
#########################
#########################

#########################
#########################
#########################

####################################################################################################
####################################################################################################
####################################################################################################

#Q4 a to c

player_guess_30 = 30
player_guess_450 = 450
player_choice_500 = 500
player_guess_950 = 950
N <- 1000
N4D <- 1:1001
oneMilN <- 1000000
loss_vector <- numeric(0)
HwQ4File <- HW2Q4 #file loaded into data frame



Q4atoc <- function(player_guess)
  
{

 
 for (i  in 1:1001 )
 {
   difference_calc <- abs(player_guess - i)**2
   loss_vector = append(loss_vector, difference_calc)
  
   
   
 }
  Mean_loss_vec <- mean(loss_vector)
  print(Mean_loss_vec)
  
 
} #End Function

Q4atoc(player_guess_30)
Q4atoc(player_guess_450)
Q4atoc(player_guess_950)


####################################################################################################
####################################################################################################
####################################################################################################

#Q4D

Q4D <- function(player_num)
{
  
  #get all the values from 0:1000 and then random generate the values from the pool of OneMilN
  
  random_generator_4D <- sample(player_num, oneMilN, TRUE)
  loss_vector_4D <- numeric(0)
  
  
  for (i  in 1:length(N4D) )
  {
    mean_guess <- mean(random_generator_4D)
    difference_calc <- abs(mean_guess - i)**2
    loss_vector_4D = append(loss_vector_4D, difference_calc)
    
    
  }
  
  final_min <- which.min(loss_vector_4D)
  print(final_min)
  return(final_min)
} #End Q4D




####################################################################################################
####################################################################################################
####################################################################################################

#Q4E

etest<- Q4atoc(500)

print(etest)

#Q4F

Q4F <- function(player_num)
{
  loss_vector_4f <- numeric(0)
  storage_df_1 <-  data.frame()
  #get all the values from 0:1000 and then random generate the values from the pool of OneMilN
  
  random_generator_4f <- sample(player_num, oneMilN, TRUE)

  
  for (i  in 1:length(player_num) )
  {
    mean_guess <- mean(random_generator_4f)
    #print(mean_guess)
    difference_calc <- abs(mean_guess - i)**2
    loss_vector_4f = append(loss_vector_4f, difference_calc)
    storage_df <- data.frame(vectorFile = player_num[i], Expected_Loss = loss_vector_4f)
    storage_df_1 <- bind_rows(storage_df_1, storage_df)
    
    
  }
  
  final_min <- storage_df_1$vectorFile[which.min(storage_df_1$Expected_Loss)]
  print(final_min)
  return(final_min)
} #End Q4F


I_am_Over_This <-  Q4F(HwQ4File)

print(I_am_Over_This)





