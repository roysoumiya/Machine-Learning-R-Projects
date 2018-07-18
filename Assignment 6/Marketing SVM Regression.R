## http://dni-institute.in/blogs/svm-for-regression-using-r/

cash <-read.csv(file="C:/MY FILES/Data/Marketing/cash_collection.csv",
                stringsAsFactors=F,
                header=T)
# names of variable
names(cash)
 
## [1] "MonthYear"       "days"            "Future_Due"      "Past_Due"       
## [5] "Due_in_month"    "Cash_collection"
 
# Summary
summary(cash)
 
##   MonthYear              days         Future_Due        Past_Due    
##  Length:51          Min.   :25.00   Min.   : 196.3   Min.   :318.7  
##  Class :character   1st Qu.:28.00   1st Qu.: 399.4   1st Qu.:447.2  
##  Mode  :character   Median :30.00   Median : 526.2   Median :554.2  
##                     Mean   :30.41   Mean   : 543.0   Mean   :549.4  
##                     3rd Qu.:33.00   3rd Qu.: 618.5   3rd Qu.:619.5  
##                     Max.   :38.00   Max.   :1118.6   Max.   :923.0  
##   Due_in_month    Cash_collection 
##  Min.   : 661.5   Min.   : 930.6  
##  1st Qu.: 926.3   1st Qu.:1085.3  
##  Median :1003.6   Median :1155.4  
##  Mean   :1140.2   Mean   :1230.2  
##  3rd Qu.:1420.7   3rd Qu.:1400.8  
##  Max.   :1700.9   Max.   :1710.7

# Regression model
reg.model <- lm(Cash_collection~Future_Due,
                data=cash)
 
#Plot
plot(Cash_collection~Future_Due,
     data=cash,
     pch=16,
     col="blue",
     xlab="Future Due Value",
     ylab="Cash Collection",
     main="Regression Model")
# Add the fitted line
abline(reg.model,
       col="red")

# Forecasted cash collection
 
cash$forecast.cash.collected <- predict(reg.model,
                                   data=cash)
 plot.new()
#Plot Forecasted Values
points(cash$Future_Due, 
       cash$forecast.cash.collected, 
       col = "orange",
       pch=18)

# R Square of a Regression Model
 
summary(reg.model)$r.squared
## [1] 0.4748865
summary(reg.model)$adj.r.squared
## [1] 0.4641699

# Square of Error
cash$squaredError.reg <- (cash$forecast.cash.collected-cash$Cash_collection)^2
 
MSE.Regression <- mean(cash$squaredError.reg)

# Package for SVM
library(e1071)
svm.model <- svm(Cash_collection~Future_Due,
                data=cash,
                type="nu-regression",
                kernel="radial"
                )
 
cash$forecast.cash.collected.svm <- predict(svm.model,
                                            data=cash)
 
#Plot
plot(Cash_collection~Future_Due,
     data=cash,
     pch=16,
     col="blue",
     xlab="Future Due Value",
     ylab="Cash Collection",
     main="SVM Model")
 
points(cash$Future_Due, 
       cash$forecast.cash.collected.svm, 
       col = "green", 
       pch=17)

# Square of Error - Support Vector Machine
cash$squaredError.svm <- (cash$forecast.cash.collected.svm-cash$Cash_collection)^2
 
MSE.SVM <- mean(cash$squaredError.svm)
 

[1] 20009.39
