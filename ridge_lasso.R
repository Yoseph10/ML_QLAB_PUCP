
####### RIDGE AND LASSO

rm(list=ls()) 

library(ISLR)  

View(Hitters)

names(Hitters) 

dim(Hitters)


# Shows the number of observations with missing values /glmnet does not accept missing values
sum( is.na(Hitters$Salary) )

# deleting missing obs
Hitters = na.omit(Hitters)

dim(Hitters)

sum( is.na(Hitters) )

x = model.matrix(Salary~.,Hitters)[,-1] #all independent vars

y = Hitters$Salary  #outcome 



#-------------------------------
# Ridge
#-------------------------------


library(glmnet)

## Define the set of λ values
# The highest values of λ are at the beginning
grid = 10^seq( 10 ,-2 ,length=100 )


# y = BETA0 +  BETA1*X + BETA2*X2 + .... +BETA19*X19 + λ 

#glmnet by default standardizes all vars 
ridge.mod = glmnet(x,y,alpha=0, lambda=grid)  #alpha 0 = ridge 

dim( coef(ridge.mod) )  #100 λ's values, for each λ there are 20 estimated coefficients (19 vars + intercept)

 

## What happens to the coefficients when λ changes? 
# If λ is high, the coefficients will tend to be zero  
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

coef(ridge.mod)[,100]

sqrt( sum(coef(ridge.mod)[-1,50]^2) )  #without the intercept


ridge.mod$lambda[60]
coef(ridge.mod)[,60]


sum(coef(ridge.mod)[-1,60]^2) 

sum(coef(ridge.mod)[-1,98]^2) 

sum(coef(ridge.mod)[-1,1]^2)


# We can obtain the ridge regression coefficients for a new value of λ, say 50
predict(ridge.mod, s= 10^10 , type="coefficients" )[1:20,]



## Now we divide the sample into two groups

# Training and Test Set (or validation)
set.seed(1)

train = sample( 1:nrow(x) , nrow(x)/2) #half of the obs

test = (-train)   

y.test = y[test] 


## We implement the ridge regression over all possible values of λ, but using training sample
ridge.mod = glmnet( x[train,] ,y[train], alpha=0 , lambda=grid , thresh=1e-12)


## Predicting with the trained model on the test set for λ=4
ridge.pred = predict( ridge.mod , s=4 , newx=x[test,])


#Test MSE
mean((ridge.pred - y.test)^2) ## 142199.2


ridge.pred=predict(ridge.mod, s=1e10,newx=x[test,]) #with high λ

mean((ridge.pred-y.test)^2)


ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train]) #with λ=0

mean((ridge.pred-y.test)^2)



## WITH WHICH λ IS OBTAINED THE LOWEST MSE (MEAN SQUARED ERROR)?

# Cross validation

dim(x[train,])
length(y[train])

set.seed(1)


## Carrying out the ridge regression for different values of λ

cv.out = cv.glmnet( x[train,] , y[train] , alpha=0 )

plot(cv.out)

bestlam = cv.out$lambda.min

bestlam  #the best  λ -> min MSE


## We predict the ridge value with the best λ (in the test set)
ridge.pred = predict( ridge.mod , s = bestlam , newx=x[test,] ) 

mean((ridge.pred-y.test)^2)


## Finally, the model is reestimated using all the observations in the sample and the value of λ selected
out = glmnet( x , y , alpha=0 )

predict(out , type="coefficients", s=bestlam )[1:20,]



#-------------------------------
# The Lasso
#-------------------------------


lasso.mod = glmnet( x[train,] , y[train] , alpha=1 , lambda=grid )

set.seed(1)


cv.out = cv.glmnet( x[train,] , y[train] ,alpha=1 )

plot(cv.out)

bestlam=cv.out$lambda.min



lasso.pred=predict( lasso.mod , s=bestlam , newx=x[test,]) 

mean((lasso.pred-y.test)^2)


out=glmnet( x , y , alpha=1, lambda=grid ) 
lasso.coef = predict( out , type="coefficients" , s=bestlam )[1:20,]
lasso.coef

lasso.coef[lasso.coef!=0]

