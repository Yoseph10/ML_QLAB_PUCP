
####### CROSS-VALIDATION AND BOOSTRAP


#####################
### Cross-Validation
####################

rm(list=ls())

library(ISLR)

# We will try to predict mpg (miles per gallon)


#-------------------------------
# 1. The validation Set Approach
#-------------------------------

set.seed(1)

str(Auto)

dim(Auto)

# Dividing our sample
train = sample(392,196)  #without replacement 


# Running our regression with training data
lm.fit = lm( mpg ~ horsepower, data=Auto , subset=train )
lm.fit$coefficients


attach(Auto) #Objects in the database can be accessed by simply giving their names


# MSE (mean squared error) 
mean( ( mpg - predict(lm.fit,Auto) )[-train]^2 ) 


# Analogous but with polynomial of degree 2 (more complexity)

# mpg = BETA1*horsepower + BETA*horsepower^2

lm.fit2=lm( mpg ~ poly(horsepower,2), data=Auto , subset=train )

mean( ( mpg - predict(lm.fit2,Auto) )[-train]^2 )


# Analogous but with polynomial of degree 3
lm.fit3=lm( mpg ~ poly(horsepower,3), data=Auto ,subset=train ) 

mean( ( mpg-predict(lm.fit3,Auto) )[-train]^2 )



# A different random selection
set.seed(2)

train = sample(392,196)

lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)



#-------------------------------
# 2. Leave-One-Out Cross-Validation
#    (LOOCV)
#-------------------------------


#There is nothing random here 

#lGLM models allow us to build a linear relationship between the response and predictors, 
#even though their underlying relationship is not linear

glm.fit = glm( mpg ~ horsepower, data=Auto ) 
coef(glm.fit)

lm.fit=lm( mpg ~ horsepower, data=Auto )
coef(lm.fit)


library(boot)

glm.fit = glm( mpg ~ horsepower , data=Auto )

#It gives us the LOOCV error by default 
cv.err = cv.glm( Auto , glm.fit )

#Delta delivers cross validation error 
#delta[1] for LOOCV and delta[2] for K-Groups
cv.err$delta

cv.err$delta[1]

cv.error = rep(0,5)


# mpg = BETA1*horsepower 

# mpg = BETA1*horsepower + BETA*horsepower^2

# mpg = BETA1*horsepower + BETA*horsepower^2 + BETA*horsepower^3

# mpg = BETA1*horsepower + BETA*horsepower^2 + BETA*horsepower^3 + BETA*horsepower^4

# mpg = BETA1*horsepower + BETA*horsepower^2 + BETA*horsepower^3 + BETA*horsepower^4 + BETA*horsepower^5

for (i in 1:5){
        
        glm.fit = glm( mpg ~ poly(horsepower,i), data=Auto )
        cv.error[i] = cv.glm( Auto , glm.fit )$delta[1]
        
}

cv.error



#-------------------------------
# 3. K-Fold Cross-Validation
#-------------------------------


#There is randomness here

set.seed(17)

cv.error.10 = rep(0,10)

for (i in 1:10){
        
        glm.fit = glm( mpg ~ poly(horsepower,i), data=Auto )
        cv.error.10[i] = cv.glm( Auto , glm.fit , K=10 )$delta[2]
        
} 

cv.error.10



#####################
### The Bootstrap
####################


alpha.fn=function(data,index){
        X=data$X[index]
        Y=data$Y[index]
        return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)

alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio,alpha.fn,R=1000)


# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index)
        return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto,1:392)

set.seed(1)

boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn=function(data,index)
        coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)

boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef