#1. Input data
Targets <- read.csv("E:/Self-Learning/Data Analytics/R Program/Practice/Preppin Data/day 03/Targets.csv")
View(Targets)
Input <- read.csv("E:/Self-Learning/Data Analytics/R Program/Practice/Preppin Data/day 03/PD 2023 Wk 1 Input (1).csv")
View(Input)

#2. For the transactions file:

#2.1 Filter the transactions to just look at DSB
#These will be transactions that contain DSB in the Transaction Code field
task2 <- data.frame(Trans.code = sapply(strsplit
                                 (Input$Transaction.Code, "-"), function(x) x[1]),
                   Input$Transaction.Code)
View(task2)

#2.2 Rename the values in the Online or In-person field, 
#Online of the 1 values and In-Person for the 2 values
Input <- Input %>%
  mutate(Online.or.In.Person = case_when(
    Output_og$Online.or.In.Person == "Online" ~ "1",
    Output_og$Online.or.In.Person == "In-person" ~ "2",
    TRUE ~ as.character(Output_og$Online.or.In.Person)    
  ))

#2.3 Change the date to be the quarter 
library("lubridate")
Input$Transaction.Date <- month(Input$Transaction.Date)

Input <- Input %>%
  mutate(Transaction.Date = case_when(
    Input$Transaction.Date >= 1 & Input$Transaction.Date < 4 ~ "Q1",
    Input$Transaction.Date >= 4 & Input$Transaction.Date < 7 ~ "Q2",
    Input$Transaction.Date >= 7 & Input$Transaction.Date < 10 ~ "Q3",
    Input$Transaction.Date >= 10 & Input$Transaction.Date < 13 ~ "Q4"
  ))

#2.4 Sum the transaction values for each quarter and for each Type of Transaction 
#(Online or In-Person)
task2.4<- Input %>%
  group_by(Transaction.Date, Online.or.In.Person) %>%
  summarise(Value = sum(Value))

#3. For the targets file
#3.1 Pivot the quarterly targets so we have a row for each 
#Type of Transaction and each Quarter
#Rename the fields

library(tidyr)
task3.1 <- Targets %>%
  pivot_longer(cols = starts_with("Q"),
               names_to = "Quarter",
               values_to = "Targets")

#3.2 Remove the 'Q' from the quarter field and make the data type numeric 

library("stringr")  
task3.1$Quarter <- str_remove_all(task3.1$Quarter,"Q")
task2.4$Quarter <- str_remove_all(task2.4$Transaction.Date,"Q")

task2.4$Online.or.In.Person <- str_replace_all(task2.4$Online.or.In.Person, "1", "Online")
task2.4$Online.or.In.Person <- str_replace_all(task2.4$Online.or.In.Person, "2", "In-Person")
task2.4= task2.4[,2:4]

#4. Join the two datasets together
task4 <- merge(task2.4, task3.1, by = c("Quarter", "Online.or.In.Person"), all = TRUE)

#6. Calculate the Variance to Target for each row 
task4$Variance <- task4$Value- task4$Targets
write.csv(task4, "E:/Self-Learning/Data Analytics/R Program/Practice/Preppin Data/day 03/Output.csv")




