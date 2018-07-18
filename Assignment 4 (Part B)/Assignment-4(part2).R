                                                #ASSIGNMENT 4 [PART 2]
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(dplyr)
library(glmnet)
?c #Documentation

#-----------------------

#Generate radians value from 60 to 300 degree in steps of 4:
x = data.frame()
for(i in seq(60,300,4))
  
#Use rbind function to store in x datadrame:
{ 
 x<- rbind(x,(i*pi/180))
}

head(x)
tail(x)

nrow(x)
n <- seq(0,0.15,length.out = 61)
n
c.norm <- rnorm(n)
y <- sin(x)+c.norm # Gaussian/normal error
y

data<- cbind(x,y)
data
colnames(data)<- c("X", "Y")
data

plt<-plot(data)

  X_2<- data$X^2
  X_3<- data$X^3
  X_4<- data$X^4
  X_5<- data$X^5
  X_6<- data$X^6
  X_7<- data$X^7
  X_8<- data$X^8
  X_9<- data$X^9
  X_10<- data$X^10
  X_11<- data$X^11
  X_12<- data$X^12
  X_13<- data$X^13
  X_14<- data$X^14
  X_15<- data$X^15
datax<- cbind(data,X_2,X_3,X_4,X_5,X_6,X_7,X_8,X_9,X_10,X_11,X_12,X_13,X_14,X_15)
head(datax)

# Fitting Model and predicting
fit<-lm(Y ~ X ,data = datax)
coeff=coefficients(fit)
print(fit)
summary(fit)
coef(summary(fit))
print(fit)
summary(fit)
y_pred<-fitted(fit)
y_pred
coeff

# Calculating Residuals
rss<-sum((y_pred - data$Y)^2)
m1<-cbind(rss,coeff)
m1

# For x_2
fit2<-lm(Y ~ X,X_2 ,data = datax)
coeff2=coefficients(fit2)
y_pred2<-fitted(fit2)
rss2<-sum((y_pred2 - data$Y)^2)
m2<-cbind(rss2,rss,coeff,coeff2)
m2

# For x_6
fit3<-lm(Y ~ X,X_2,X_3 ,data = datax)
coeff3=coefficients(fit6)
y_pred3<-fitted(fit6)
rss3<-sum((y_pred3 - data$Y)^2)
m3<-cbind(rss2,rss,coeff,coeff2,rss3,coeff3)
m3

#For X_15
fitx<-lm(Y ~. ,data = datax)
coeffx=coefficients(fitx)
y_predx<-fitted(fitx)
rssx<-sum((y_predx - data$Y)^2)

# Combined Coefficients of all 4 models
mx<-cbind(rss2,rss,coeff,coeff2,rss3,coeff3,rssx,coeffx)
mx

#-----------------------

    #RIDGE Implementation:

?glmnet #Documentation

x <- datax[,-2]
y<-datax[,2]
y

ridge.mod <- glmnet(as.matrix(x), y, alpha = 0)
y_predR<-predict(ridge.mod, s = 0,type=c("coefficients"))
ridge.rss<-sum((y_predR - y)^2)
#Co-efficients of RIDGE
coefficients(ridge.mod)

#-----------------------

    #LASSO Implementation

x <- datax[,-2]
y<-datax[,2]
y

lasso.mod <- glmnet(as.matrix(x), y, alpha = 1)
y_predL<-predict(lasso.mod, s = 0,type=c("coefficients"))
y_predL
#Co-efficients of lasso
coefficients(lasso.mod)
