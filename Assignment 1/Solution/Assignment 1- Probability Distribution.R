                                        #ALL PROBABILITY DISTRIBUTIONS   


  #1. NORMAL (GAUSSIAN) DISTRIBUTION:

#USE CASE: Assuming that the test scores of a college entrance exam fits a normal distribution. 
#where mean test score is 72, and the standard deviation is 15.2.
#Calculate the percentage of students scoring 84 or more in the exam.

#P(X=60)
pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 
plot(pnorm)
# what is the third quartile of the marks?
#qnorm
valueq=qnorm(p=0.90,mean=72,sd=15.2)
valueq
plot(valueq)
hist(valueq)
abline(v=30)
hist(valueq, col="red")
# what is the random marks of 40 students?
valuer=rnorm(n=40, mean=72,sd=15.2)
valuer
plot(valuer)
hist(valuer,col="blue")

#---------------------------------------------------
#---------------------------------------------------

    #2. BINOMIAL DISTRIBUTION:

#USE CASE: Rolling of a dice when the trials are 20 and success of getting 
#a number is 2 and probability is 1/6

#P(X<=3)
pbinom(q=3, size=20, prob=1/6, lower.tail=F)
t=pbinom(q=2, size=20, prob=1/6, lower.tail=T)+ pbinom(q=3, size=20, prob=1/6, lower.tail=F)
# random function
set.seed(20)
rbinom(n=10,5,prob=1/6)+1
#quantile function
#10th percentile
qbinom(p=0.1,10,1/2)


#---------------------------------------------------
#---------------------------------------------------


    #3. BERNOULLI DISTRIBUTION

#USE CASE: rolling of a dice when the trials are 20 and success of getting 
#a number is 2 and probability is 1/6
# density function2), 20, 1/6)
#P(X<=3)
#distribution function
pbinom(q=3, size=20, prob=1/6, lower.tail=F)
t=pbinom(q=2, size=20, prob=1/6, lower.tail=T)+ pbinom(q=3, size=20, prob=1/6, lower.tail=F)
# random function
set.seed(20)
rbinom(n=10,5,prob=1/6)+1
#quantile function
#10th percentile
qbinom(p=0.1,10,1/2)

#---------------------------------------------------
#---------------------------------------------------


    #4. EXPONENTIAL DISTRIBUTION

#USE CASE: Suppose the mean checkout time of a supermarket cashier is three minutes.
#Find the probability of a customer checkout being completed by the cashier in less than two minutes.

exp=pexp(2, rate=1/3) 
exp
hist(exp)

#---------------------------------------------------
#---------------------------------------------------


    #5. POISSON DISTRIBUTION:

#USE CASE: On a particular river, overflow floods occur once every 100 years on average.
#Calculate the probability of k = 0, 1, 2, 3, 4, 5, or 6 overflow floods in a 100-year interval, assuming the Poisson model is appropriate.
#Because the average event rate is one overflow flood per 100 years, λ = 1

#P(X=2)
dpois(x=1,lambda = 2)
dpois(x=1:6,lambda = 2)
ppois(q=1,lambda=2,lower.tail = T)
rpois(n=1, lambda=2)
# 10th percentile
hist(qpois(p=0.10,lambda =2 ))


#---------------------------------------------------
#---------------------------------------------------



    #6. STUDENT'S T DISTRIBUTION

#USE CASE: Find the 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom.

#p=.025
#p=0.975
#df=5
hist(qt(c(.025, .975),df=5)) # 5 degrees of freedom 

#---------------------------------------------------
#---------------------------------------------------


    #7. CHI-SQUARED DISTRIBUTION
#USE CASE: Find the 95th percentile of the Chi-Squared distribution with 7 degrees of freedom.

#Quantile function: qchisq
chi=qchisq(0.95, df=7)         #p: 0.95, degrees of freedom: 7 
hist(chi)
?qchisq #for documentation

#---------------------------------------------------
#---------------------------------------------------


    #8. CONTINUOUS UNIFORM DISTRIBUTION

#USE CASE: Select ten random numbers between one and three.

uniform=runif(x=10, min=1, max=3) 
uniform
hist(uniform)
?runif #for documentation

#---------------------------------------------------
#---------------------------------------------------


    #9. GEOMETRIC DISTRIBUTION

#USE CASE: Four roads start from a junction. Only one of them leads to a mall. 
#The remaining roads ultimately lead back to the starting point. 
#A person not familiar with these roads wants to try the different roads one by one to reach the mall.
#What is the probability that his second attempt will be correct?

x=2-1
geom=dgeom(1,0.25)
geom
hist(geom)

#---------------------------------------------------
#---------------------------------------------------

    #10. HYPERGEOMETRIC DISTRIBUTION

#USE CASE: Suppose you have an urn with 30 balls, 10 red and 20 white (using the Larsen and Marx language. You select 15 at random. What is the probability that the sample contains 8 red? contains 8 or more red?

x=8
m=10
n=20
k=15
dhyper(8,10,20,15)

#---------------------------------------------------
#---------------------------------------------------


    #11. LOG NORMAL DISTRIBUTION

#USE CASE: Get 20 randomly generated numbers from a lognormal distribution with the geometric mean of 10 and geometric standard deviation of 2.5.
x=20
meanlog=log(10)
sdlog=log(2.5)
plot(rlnorm(20, log(10), log(2.5)))


#---------------------------------------------------
#---------------------------------------------------


    #12. WEIBULL DISTRIBUTION

#USE CASE: I have to simulate a system's fail times, to do so I have to use the Weibull distribution with a "decreasing hazard rate" and a shape of "0.7-0.8".
#I have to generate a file with 100 results for the function that uses random numbers from 0 to 1.

barplot(pweibull(q, shape=0.7:0.8, scale = 1, lower.tail = T, log.p = F))
?pweibull #for documentation

#---------------------------------------------------
#---------------------------------------------------


    #13. BETA DISTRIBUTION

#USE CASE: Calculate a 95% confidence intervals for seller A and B for their true unknown rating level.

# 1. 95%-CI for seller A (9 of 10 are good)
x=0.025
shape1=0.975
shape2=9
ncp=1
qbeta(c(0.025,0.975),9+1,1+1)

# 2. 95%-CI for seller B (400 of 500 are good)
x=0.025
shape1=0.975
shape2=400
ncp=100
qbeta(c(0.025,0.975),400+1,100+1)


#---------------------------------------------------
#---------------------------------------------------


    #14. GAMMA DISTRIBUTION

#USE CASE: Calculating the probability for the distribution in R:
#P(X < 1) =? given that α = 2 and β = 0.5

gamma=pgamma (q=1 , shape =2 , scale =0.5 , lower.tail = TRUE)
gamma
hist(gamma)
?pgamma #for documentation

#---------------------------------------------------
#---------------------------------------------------


    #15. NEGATIVE BINOMIAL DISTRIBUTION

#USE CASE: The probability of obtaining the fourth cross before the third head (and then after two head) is equal to 11,72%.

x=1:25
size=2
prob=0.5
barplot(dnbinom(1:25,2,0.5), col="grey", names.arg=1:25)

#---------------------------------------------------
#---------------------------------------------------

