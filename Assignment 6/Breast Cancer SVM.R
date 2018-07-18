## https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM
## DATA: http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
# install.packages('e1071', dependencies = TRUE)
library(e1071)
library(MASS)
library(ggplot2)

dataset <- read.csv('Desktop/wdbc.data.txt', head = FALSE)
index <- 1:nrow(dataset)
testindex <- sample(index, trunc(length(index)*30/100))
testset <- dataset[testindex,]
trainset <- dataset[-testindex,]
names(dataset)

tuned <- tune.svm(V2~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)

model  <- svm(V2~., data = trainset, kernel = "radial", gamma = 0.001, cost = 10)
summary(model)

## Testing

prediction <- predict(model, testset[,-2])
tab <- table(pred = prediction, true = testset[,2])
tab

