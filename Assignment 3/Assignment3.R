#PART 2:
#Implement:
#(i) Decision tree classification with Titanic dataset (predict Survived) 
#(ii) Decision tree regression Energy efficiency Dataset (outcome y1 or y2) in R. 

#----------------------------------------------------------------------------

  #(i) TITANIC DATASET:

# Check if XLConnect is already installed:
any(grepl("XLConnect", 
          installed.packages()))

#If FALSE, install package XLConnect:
install.packages("XLConnectJars")
install.packages("XLConnect")
install.packages("rJava")
install.packages("rpart")
install.packages("rpart.plot")
#or (used this)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(XLConnectJars)
library(XLConnect)
library(rJava)

#Read and display titanic train and test data from CSV:
library(XLConnect)
titanic_train=read.csv("Desktop/Titanic_train.csv") 
titanic_train

titanic_test=read.csv("Desktop/Titanic_test.csv") 
titanic_test

#Documentation for rpart and rpart.plot:
?rpart
?rpart.plot
?prp

#Display tree using the rpart library:
library(rpart)
library(rpart.plot)
tree_model=rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, 
                 data=titanic_train, method="class")
tree_model
prp(tree_model)
rpart.plot(tree_model)


#Prediction of Survived using predict method:
Prediction_data <- predict(tree_model, titanic_test, type = "class")
submit_data <- data.frame(Name = titanic_test$Name, Survived = Prediction_data)
submit_data


#----------------------------------------------------------------------------

  #(ii) ENERGY EFFICIENCY DATASET:

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

#Read and display Energy efficiency data from Excel sheet:
library(XLConnect)
energy_data = readWorksheetFromFile("Desktop/Energy_Efficiency.xlsx",sheet = 1)
energy_data

#Extract 80% value of the energy dataset:
energy_train<-floor(0.8 * nrow(energy_data))
energy_train

#Set the seed to 13:
set.seed(13)
train_ind<-sample(seq_len(nrow(energy_data)),size = energy_train)
train_ind

train_data = energy_data[train_ind,]
train_data
test_data = energy_data[-train_ind,]
test_data

modelY2 = rpart(Y2~ X1+X2+X3+X4+X5+X6+X7+X8, data = train_data, method="anova")
modelY2
modelY1 = rpart(Y1~ X1+X2+X3+X4+X5+X6+X7+X8, data = train_data, method="anova")
modelY1

#Predict the model:
p1= predict(modelY2,test_data,type = "vector")
p= predict(modelY1,test_data,type = "vector")

#Plot the data model:
plot(p1,type='h',col="green",ylim=c(10,max(p1,p)))
lines(p,type='h',lty=2,col="red")

