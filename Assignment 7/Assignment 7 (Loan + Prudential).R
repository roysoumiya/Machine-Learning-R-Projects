                                  #ASSIGNMENT 7: NEURAL NETWORKS

                                    #PART 1: LOAN DATASET

install.packages("XLConnectJars")
install.packages("XLConnect")
install.packages("rJava")
install.packages("neuralnet")

library(XLConnectJars)
library(rJava)

#Read and display Loan data from Loan CSV:
train <-read.csv("Desktop/loan.csv", sep= ",")
train
str(train) #14 variables and 429 observations
summary(train)

#Find out how many rows are NA in each column
#array(X, Margin, Fun..)
#where: Margin- a vector giving the subscripts which the function will be applied over where 1 = rows, 2 = columns
apply(train,2,function(x) sum(is.na(x))) #There are no missing values


#Reducing the number of observations
train_reduced <- train[1:350,]
train_reduced
head(train_reduced)
test <- train[351:429,]
test

names <- names(train_reduced) #Displayes all the column headers
names 

#Converting only the required columns to numeric
Res_status<-as.numeric(train_reduced$Res_status)
Occupation<-as.numeric(train_reduced$Occupation)
Job_status<-as.numeric(train_reduced$Job_status)
Acc_ref<-as.numeric(train_reduced$Acc_ref)
Liab_ref<-as.numeric(train_reduced$Liab_ref)
Decision<-as.numeric(train_reduced$Decision)

#Creating a dataframe to input the numeric-converted values
dataframe <-c ('Decision','Res_status','Occupation','Job_status',
        'Liab_ref','Acc_ref','Balance')
dataframe

#Building the Neural Network (NN) model and pushing it in the dataframe
library(neuralnet)
NeuralModel <- neuralnet(Decision ~ Res_status+Occupation+Job_status+
                      Liab_ref+Acc_ref+Balance, data=dataframe,
                    hidden = 3, lifesign = "minimal", 
                    linear.output = FALSE, threshold = 0.1)

#Displaying the graph for neural networks model
plot(model2,rep="best")

#---------------------------------------------------------------------------------------------


                              #PART 2: PRUDENTIAL LIFE INSURANCE DATASET


install.packages("XLConnectJars")
install.packages("XLConnect")
install.packages("rJava")
install.packages("neuralnet")

library(XLConnectJars)
library(rJava)

#Read and display data from Prudential CSV 
#[The data was preprocessed and exported the cleaned data during project]:

Final_Training_Data<-read.csv("Desktop/Training_Data.csv", sep= ",")
Final_Training_Data
str(Final_Training_Data)
summary(Final_Training_Data)

Final_Test_Data<-read.csv("Desktop/Test_Data.csv", sep= ",")
Final_Test_Data
str(Final_Test_Data)
summary(Final_Test_Data)

Final_Training_Data = Final_Training_Data[, c(1:6, 53)]
str(Final_Training_Data)

Final_Test_Data = Final_Test_Data[, -c(1:6, 53)]
str(Final_Test_Data) 

#Building Neural Networks model
library(neuralnet)
neural_model = neuralnet(response ~ Product_Info_4 + Ins_Age + BMI + Family_Hist_2 + Family_Hist_4, 
                         data = Final_Training_Data, hidden = 3, lifesign = "minimal", 
                         linear.output = FALSE, threshold = 0.1)

#Displaying the graph for neural networks model
plot(neural_model,rep="best")

