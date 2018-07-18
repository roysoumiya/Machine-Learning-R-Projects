                                                #ASSIGNMENT 2

#PART 1
#Linear regression: 
#Run the example linear regression in R. 
#Please compute the value of F0 (211.9) separately step-by-step, either in Excel or in R, 
#and then arrive at the same result you obtained by running summary in the regression example.


#First time set up:
options('java.home')
options("java.home"="/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre")
Sys.setenv("LD_LIBRARY_PATH"='$JAVA_HOME/jre/lib/server')
Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre')

#Empty workspace
rm(list=ls())

#Get Working Directory
getwd()

# Check if XLConnect is already installed:
any(grepl("XLConnect", 
          installed.packages()))

#If FALSE, install package XLConnect:
install.packages("XLConnectJars")
install.packages("XLConnect")
install.packages("rJava")
#or (used this)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(XLConnectJars)
library(XLConnect)
library(rJava)

#Read and display Loan data from Excel sheet:
library(XLConnect)
LinearData<- readWorksheetFromFile("Desktop/LinearData.xlsx", sheet = 1)
LinearData

#Create Model
LinearModel <- lm(Yield..yi. ~ Factor.1..xi1. + Factor.2..xi2., data=LinearData)

#Show Results
summary(LinearModel) 


#Compute value of F0 (211.9):

#Create Matrix X (independent variables) and y (dependent variables)
X <- matrix(c(1, 1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,41.9, 43.4, 43.9, 44.5, 47.3, 47.5, 47.9, 50.2, 52.8, 53.2, 56.7, 57.0, 63.5, 65.3, 71.1,77.0,77.8,
             29.1, 29.3, 29.5, 29.7, 29.9, 30.3, 30.5, 30.7, 30.8, 30.9, 31.5, 31.7, 31.9, 32.0, 32.1, 32.5, 32.9),  ncol = 3) 
X

y <- matrix(c(251.3, 251.3, 248.3, 267.5, 273.0, 276.5, 270.3, 274.9, 285.0, 290.0, 297.0, 302.5, 304.5, 309.3, 321.7, 330.7, 349.0), ncol = 1)
y

#Least Square Estimate:
LSE1 <- (t(X)%*%X)
LSE1

invLSE <- solve(LSE1)
invLSE

LSE2 <- t(X)%*%y
LSE2

LSE <- invLSE%*%LSE2
LSE

#Fitted Regression Model (observed fifth response value):

FRM <- LSE[1,1] + LSE[2,1]%*%47.3 + LSE[3,1]*29.9
FRM

#Fifth value of y(yield) matrix:
FifthValue <- y[5,1]
FifthValue

#Residual Value:
RV <- FifthValue - FRM
RV

#Fitted Regression Model (new values):

FRM2 <- LSE[1,1] + LSE[2,1]%*%47 + LSE[3,1]*31
FRM2

#Find out Regression Mean Square:

#Value of H:
H <- X%*%solve((t(X)%*%X))%*%t(X)
H
#Value of J:
J <- matrix(1:1, 17, 17)
J

#Value of SSR:
SSR <- t(y)%*%(H-(1/17)*J)%*%y
SSR

#Value of MSR:
MSR <- SSR / 2  #Degree of freedom (given): 2
MSR

#Value of SSE:
#I is identity matrix of order n
I <- diag(17)
I
  
SSE <- t(y)%*%(I - H)%*%y
SSE

#Value of MSE:
n=17
k=2
MSE <- SSE/14
MSE

#Value of f0:
f0 <- MSR/MSE
f0




#---------------------------------------------------------------------------------------------



#PART 2
#Logistics regression: 
#Run logistics regression on the loan example with the variable Decision as the dependent variable 
#and the five categorical variables identified in the 
#class (Res_status, Occupation, Job_status, Liab_ref, Acc_ref) as the independent variables. 
#Show your prediction for input (owner, creative_, governmen, f, given) 
#and (rent, creative_, governmen, f, given).

#First time set up:
options('java.home')
options("java.home"="/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre")
Sys.setenv("LD_LIBRARY_PATH"='$JAVA_HOME/jre/lib/server')
Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre')

#Empty workspace
rm(list=ls())

#Get Working Directory
getwd()

# Check if XLConnect is already installed:
any(grepl("XLConnect", installed.packages()))

#If FALSE, install package XLConnect:
install.packages("XLConnectJars")
install.packages("XLConnect")
install.packages("rJava")

#or (used this)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(XLConnectJars)
library(XLConnect)
library(rJava)

#Read and display Loan data from Excel sheet:
library(XLConnect)
loan<- readWorksheetFromFile("Desktop/loan.xlsx", sheet = 1)
loan

#Structure of loan dataset: 14 Variables and 429 Observations 
str(loan)

loan$Decision= as.factor(loan$Decision)
contrasts(loan$Decision)

loan$Res_status= as.factor(loan$Res_status)
contrasts(loan$Res_status)

loan$Occupation= as.factor(loan$Occupation)
contrasts(loan$Occupation)

loan$Job_status= as.factor(loan$Job_status)
contrasts(loan$Job_status)

loan$Liab_ref= as.factor(loan$Liab_ref)
contrasts(loan$Liab_ref)

loan$Acc_ref= as.factor(loan$Acc_ref)
contrasts(loan$Acc_ref)


#Create Model
LogitModel<- glm(Decision ~ Res_status + Occupation + Job_status + Liab_ref + Acc_ref, data=loan, family = "binomial")

#Show Results
summary(LogitModel) 

#Create a new dataframe:
DF1 <- data.frame(Res_status = "owner", Occupation = "creative_", Job_status = "governmen", Liab_ref = "f", Acc_ref = "given")
DF1

DF2 <- data.frame(Res_status = "rent", Occupation = "creative_", Job_status = "governmen", Liab_ref = "f", Acc_ref = "given")
DF2

#Prediction for input (owner, creative_, governmen, f, given):
Predict1 <- predict(LogitModel, DF1, type="response")
Predict1

#Prediction for input (rent, creative_, governmen, f, given):
Predict2 <- predict(LogitModel, DF2, type="response")
Predict2

