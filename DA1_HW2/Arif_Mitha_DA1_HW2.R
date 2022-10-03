# Data Analytics HW2
#Author: Arif Mitha


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

###############################################################################################################################################################################
##########################################################################READ THESE INSTRUCTIONS CAREFULLY!!!#################################################################
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
#####################################################################  QUIZ ANSWERS ##########################################################################################
###############################################################################################################################################################################

#1 -> 50
#2 -> 60
#3 -> 60
#4 -> 60
#5 -> 67



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




for (pCounter in seq(from=0, to=100, by=2)) {
 for (bCounter in seq( from=0, to=100, by=2)) {
    
    if(pCounter + bCounter <= 60)
    {
      print("b")
      print(bCounter)
   
      #break
      next
    }
    
    else
    {
      
      exam_body$P <- pCounter
      exam_body$B <- bCounter
      
      
    }#End Else
    
    
    
    for (i  in 1:rows_of_scores )
    {
      if(exam_body$exam_scores4[i] < pCounter)
      {
        
        #break
        next
      }#end IF
      
      else
      {
        #https://www.anycodings.com/1questions/465947/error-in-amplt-dataframetmp-pvalue-value-966218350888067e-05-replacement-has-1-row-data-has-0
        
        exam_body$Total[i]<- exam_body$exam_scores4 + bCounter
        
        
      }#End Else
      
    }
    
    #check for the mean of the Total and it needs to be between >70 and < 75
    if(mean(exam_body$Total) > 70 & mean(exam_body$Total < 75))
    {
      
      counter <- counter +1
      master_Data_Frame[counter, ] <- list(P_value = pCounter, B_Value = bCounter, MeanEx = mean(exam_body$Total, Std = sd(exam_body$Total)))
      
      
    }#end IF
    
    
    #If possible, choose P and B so the maximum post-retake average is between 70 and 75 (strict inequality)
    #and that P + B ≥ 60. If there are multiple P and B that satisfy this criterion, then choose the one whose
    #standard deviation of post-retake scores is closest to the standard deviation of the pre-retake scores.
  }
  
}

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
fullName_df <- data.frame()

Projects_Assignments = data.frame(Marker_ID = 1:nrow(class_roster),
                  
                  Project_Student_ID_1 = c(nrow(class_roster), 1:(nrow(class_roster)-1)),
                  
                  Project_Student_ID_2 = c((nrow(class_roster)-1):nrow(class_roster), 1:(nrow(class_roster)-2)),
                  
                  Project_Student_ID_3 = c((nrow(class_roster)-2):nrow(class_roster), 1:(nrow(class_roster)-3)))

for (i  in 1:length(Projects_Assignments))
{
  
  fullName_df_temp <- data.frame(Marker_Name = class_roster$Full_Name[Projects_Assignments$Marker_ID[i]], Project_Name_1 = class_roster$Full_Name[Projects_Assignments$Project_Student_ID_1[i]],Project_Name_2 = class_roster$Full_Name[Projects_Assignments$Project_Student_ID_2[i]], Project_Name_3 = class_roster$Full_Name[Projects_Assignments$Project_Student_ID_3[i]])
  fullName_df <- bind_rows(fullName_df, fullName_df_temp)
  
}







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

Q4D(N4D)


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






####################################################################################################
######################################## DEAD CODE #################################################
####################################################################################################

# Q3
# 
# class_roster <- data.frame(read.csv("BTMA 636 - 797 (Fall 2022).csv"))
# print(class_roster)
# class_roster <- tibble::rowid_to_column(class_roster,"Student_ID")
# 
# class_roster <- class_roster %>% unite('Full_Name', First.Name:Last.Name, remove = FALSE) 
# numbers_used_Q3 <- 1:nrow(class_roster)
# print(class_roster)
# print(numbers_used_Q3)
# 
# 
# 
# student_marking <- vector() #who is marking
# #students_used <- vector() #people being marked by Student_marking
# NQ3 <- 33 
# final_list <- data.frame()
# 
# 
# 
# if(NQ3 < 4)
# {
#   
#   print("This roster size is too small. Minimum of 4")
#   
#   
# }else
# {
#   
#   ###################################################################    
#   #Go over the whole class list
#   #for (i in 1:NQ3) 
#   for (i in 1: 6){
#     
#     
#     #Get the student who is going to be the marker
#     if(i <= 6) #NQ3
#     {
#       temp_list <- c()
#       
#       #Make sure no duplicate student markers are used 
#       while (TRUE) {
#         
#         rng_Q3 <- sample(numbers_used_Q3, 1) #pick a student to be the marker
#         
#         print(paste("Student who is marking is " , rng_Q3))
#         
#         
#         if(!rng_Q3 %in% student_marking) #check for previously used student markers
#         {
#           
#           break
#         }#end IF
#         
#         
#       } #end while
#       student_marking <- append(student_marking, rng_Q3) #This stores the list of students already picked to be markers
#       
#       ################################################   
#       students_used <- vector()
#       for (i  in 1:3)
#       {
#         
#         
#         while (TRUE)
#         {
#           rng_Q3_1 <- sample(numbers_used_Q3, 1)
#           print(paste("Students projects to be graded ", rng_Q3_1))
#           
#           
#           if(rng_Q3_1 != rng_Q3) #make sure the project being picked is the markers project
#           {
#             break
#             
#             
#             #end IF
#           }
#           
#           if(!rng_Q3_1 %in% students_used) #check to make sure the same student project isnt being picked twice for the marker
#           {
#             
#             break
#             
#           } #end IF
#           
#           temp_list <- c(temp_list, rng_Q3, rng_Q3_1)
#           print("temp list")
#           print(temp_list)
#           
#         }#END WHILE
#         
#         
#         students_used <- append(students_used, rng_Q3_1)
#         
#         
#         
#       } #END FOR LOOP 1:3
#       
#       
#     } #end IF
#     
#     
#   } #End For
#   
# }#end Else
# 
# 




# 
# 
# 
# Q4atoc <- function(player_guess)
#   
# {
#   random_generator <- sample(0:1000 , 1, replace=TRUE)
#   print(random_generator)
#   
#   #This is supposed to handle the win condition
#   
#   if(player_guess == random_generator)
#   {
#     print("You win this time Gadget! But I'll get you next time! Meow!")
#     
#   }
#   
#   #else{
#   difference_amount <- abs((player_guess +1)- random_generator) # instead of doing 2 if statements use the absolute function 200IQ, +1 to count for the inclusiveness
#   
#   difference_amount_sqr <- difference_amount**2 #square the difference
#   
#   roundToFiveGrand <- round(difference_amount_sqr/5000)*5000  #round to the nearest 5 Grand
#   
#   print("The house wins again, You owe this much:")
#   
#   print(roundToFiveGrand) #safety Check make sure the number is a single number
#   
#   if(roundToFiveGrand >= 500000)
#   {
#     print("Oof, that sucks. Looks like you might need this more than ever before. I hear they have a great benefits plan after 10 years https://careers.mcdonalds.ca/")
#   }else if(roundToFiveGrand >= 499999)
#     
#   {
#     print("That sucks, it could be worse. But you might want to bookmark this page just in case. https://careers.walmart.ca/")
#   }else 
#   {
#     print("Hope you got deep pockets, bc it only gets worse from here champ!")
#     
#   } #End else
#   
#   #} # end else
#   
# } #End Function
