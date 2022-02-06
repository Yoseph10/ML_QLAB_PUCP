
####### SIMULATIONS (training error vs test error)

rm(list=ls())


set.seed(1)

# Generating 201 observations for X
X <- seq(from = 0, to = 20, by = 0.1)


# Generating our Data Generation Model DGM
dgm <- 500 + 20*X - 90*sin(X) 



#--------------------------
# Training sample
#--------------------------

# Generating y training
y = dgm + rnorm(length(X), mean = 10, sd = 100) 


# y and X in a data frame
data = data.frame( y , X )

str(data)



#--------------------------
# Plotting training and DGM
#--------------------------

plot(X, y, col='deepskyblue4', xlab='X label', main='Datos & DGM')
lines(X, dgm, col='firebrick1', lwd=4) #DGM in red



#--------------------------
# Estimating with 3 models
#--------------------------

# We want to estimate a model which has the lowest possible error


# 1) Linear model

#y = x

model1 <- lm(y ~ X, data=data)


# 2) Polynomial model 

# y = x + x^2 + x^3 + x^4 + ..... + x^18

model2 <- lm(y ~ X + I(X^2) + I(X^3) + I(X^4) + 
                     I(X^5) + I(X^6) + I(X^7) + I(X^8) +
                     I(X^9) + I(X^10) + I(X^11) + I(X^12) +
                     I(X^13) + I(X^14) + I(X^15) +
                     I(X^16) + I(X^17) + I(X^18),
             data=data)

# 3) Nonparametric Model (Spline)

model3 <- smooth.spline(X, y, df=200) #more complexity 


# Plot: Linear model
plot(X, y, col='deepskyblue4', xlab='X', main='Linear')
abline(lm(y ~ X), col = "blue", lwd = 4)


# Plot: Polynomial model 
plot(X, y, col='blue', xlab='X',
     main='Polynomial')
lines(X, fitted(model2), col='red', lwd=4)


# Plot: Spline
plot(X, y, col='deepskyblue4', xlab='X',
     main='Spline')
lines(smooth.spline(X, y, df=200), 
      col='green', lwd=4) 



#--------------------------
# Root Mean Square Error (RMSE)
#--------------------------

rmse = function(actual, predicted) {
        
        sqrt(mean((actual - predicted) ^ 2))
}



#--------------------------
# Training Error
#--------------------------

#Predicted values
predicted1<- fitted(model1)
predicted2<- fitted(model2)
predicted3<- fitted(model3)


#RMSE 
rmse(y,predicted1)
rmse(y,predicted2)
rmse(y,predicted3)


#RMSE    values
#1     Linear   119.46405
#2   Polynomial  88.87396
#3     Spline  67.72450  ****************


#--------------------------
# Test Error
#--------------------------

#Now, with unseen data

set.seed(2)

# Generating y testing
y2 = dgm + rnorm(length(X), mean = 10, sd = 100) 


plot(X, y2, col='deepskyblue4', xlab='X', main='The Unseen 2nd Sample')


#RMSE
rmse(y2,predicted1)
rmse(y2,predicted2)
rmse(y2,predicted3)


#--------------------------
# FINAL OUTPUTS:
#--------------------------

##             Linear    Polynomial   Spline
## Seen-data   119.4640   88.87396   67.7245
## Unseen-data 123.4378  109.99681   122.3018

