
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






P <- 22
B <- 30 
S <- 21
R <- 67

Grade_Analyzer(S,R,P,B)
Grade_Analyzer(21,50,30,30) #Q1 #50
Grade_Analyzer(21,60,30,30) #Q2 #60
Grade_Analyzer(21,70,30,30) #Q3 #60
Grade_Analyzer(54,67,22,30) #Q5 #67 <- returning 84
Grade_Analyzer(54,78,22,30) #Q6 #78 <- returning 84
Grade_Analyzer(54,89,22,30) #Q7 #84

Grade_Estimator(S, P, B)
Grade_Estimator(21, 30, 30) #Q4 60
Grade_Estimator(54, 22, 30) #Q8 84
Grade_Estimator(22, 30, 30) #Q9 60
Grade_Estimator(86, 22, 30) #Q10 116

Q31to3Marker(rosterFile)



Q4atoc(player_guess_30)
Q4atoc(player_guess_450)
Q4atoc(player_guess_950)
Q4D(N4D)
etest<- Q4atoc(500)
print(etest)
I_am_Over_This <-  Q4F(HwQ4File)
print(I_am_Over_This)


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
  } #this was the error
    if(S > P)
    {
      print("score 7") 
      return(SB)
      #end IF
    }
#} was here
  }#End Function

#########################



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
  else
  {
    return(S+B)
  }
  }#End Function


        


#########################

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



