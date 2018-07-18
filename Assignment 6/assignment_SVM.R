                                        #ASSIGNMENT 6 [PART 2]
#SVM: For this small assignment, the code is already there (assignment_SVM.R), 
#you just need to explain what the code does. 
#At the end try to find the best parameters for the SVM function. 
#ALSO, run the same example in Orange. Run all the other attached SVM examples.


#--------------------------------------------------------------------------------------

######### Please fill the ??? with proper description (atleast 130 characters for each)
######### for SVM function try different values to achieve better results


#Load caret and e1071 packages
#Assign the GermanCredit dataset to a variable called dataset
library(caret) #holds functions to build predictive model
library(e1071) #to use SVM in R
data(GermanCredit)
dataset = GermanCredit 

#(ADDED)
#Print the summary and content of dataset for better understanding
summary(dataset)
dataset


#View the structure of the GermanCredit dataset
#'as.dataframe' checks whether the object is a dataframe
#lapply returns a list of the elements that corresponds to the given range of values and scales the column
#The outcome would give the list of all the rows and columns between 1 to 7 in the dataset
#Display the structure to see the output

str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)


#'sample_index' will display random 200 values out of the 1000
#Create test and train datasets with the selected data and print

sample_index = sample(1000, 200)
test_dateset = dataset[sample_index,]
test_dateset
train_dateset = dataset[-sample_index,]
train_dateset


#Create SVM to get values from the RADIAL kernel and show summary
radialmodel <- svm(Class~., kernel = "radial", data = train_dateset, ranges = list(gamma=2^(-15:3) , cost=2^(-5:15)), scale = F)
summary(radialmodel)

#Create SVM to get values from the LINEAR kernel and show summary
linearmodel <- svm(Class~., kernel = "linear", data = train_dateset, ranges = list(gamma=2^(-15:3), cost=2^(-5:15)), scale = F)
summary(linearmodel)

#Create SVM to get values from the POLYNOMIAL kernel and show summary
polymodel <- svm(Class~.,kernel = "polynomial", data = train_dateset, ranges = list(gamma=2^(-15:3), cost=2^(-5:15)), scale = F)
summary(polymodel)

#Create SVM to get values from the SIGMOID kernel and show summary
sigmoidmodel <- svm(Class~.,kernel = "sigmoid", data = train_dateset, ranges = list(gamma=2^(-15:3), cost=2^(-5:15)), scale = F)
summary(sigmoidmodel)


#We will use a generic function called 'Predict' for predictions from the results of various model fitting functions.
#The function invokes particular methods which depend on the class of the first argument.
predictions1 <-  predict(radialmodel, test_dateset[-10])
predictions1
predictions2 <-  predict(linearmodel, test_dateset[-10])
predictions2
predictions3 <-  predict(polymodel, test_dateset[-10])
predictions3
predictions4 <-  predict(sigmoidmodel, test_dateset[-10])

#'Table' function creates and displays tabular results of categorical variables from the test dataset
table(test_dateset[,10], predictions1)
table(test_dateset[,10], predictions2)
table(test_dateset[,10], predictions3)
table(test_dateset[,10], predictions4)
