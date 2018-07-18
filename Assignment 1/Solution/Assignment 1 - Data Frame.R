#First time set up:
options('java.home')
options("java.home"="/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre")
Sys.setenv("LD_LIBRARY_PATH"='$JAVA_HOME/jre/lib/server')
Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre')

#Empty workspace
rm(list=ls())

#Get Working Directory
getwd()


------------------------------------------------------
  
#Ques: Subsetting operation - 
  #Read the loan data excel file into a frame and then export only those rows
  #with age greater than 30 and unemployed to another sheet of the same excel file.

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
loan<- readWorksheetFromFile("Desktop/loan.xlsx", sheet = 1)
loan

#Subsetting operation: Age greater than 30 and unemployed
Revised_loan = subset(loan, Age >= 30.01 & Job_status != "unemploye")
Revised_loan

#Print structure to see observations and variables in the revised data frame:
str(Revised_loan)

#Export revised data frame to Excel:
library(XLConnect)
writeWorksheetToFile(file = "Desktop/loan.xlsx", data = Revised_loan, sheet = "revised_loan_data")


