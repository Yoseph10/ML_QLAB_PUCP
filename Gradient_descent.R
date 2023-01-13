
#install.packages("ISLR")


#Calculating the coefficients of a linear regression model

library(ISLR)

#Info about Hitters data: https://rdrr.io/cran/ISLR/man/Hitters.html

x = model.matrix( Salary ~. , Hitters)[ , -1]

y = na.omit(Hitters$Salary)

x <- scale(x)
y <- scale(y)


# Y = beta * X
# Y_hat = beta * X + error


#1) R package
model <- lm(y ~ x-1)

model$coefficients


#2) Manual solution

#Residual sum squared

# RSS = (Y - X %*% beta)' (Y - X %*% beta)
# RSS = t(Y) %*% Y - 2 * t(Y) %*% X %*% beta + t(beta) %*% t(X) %*% X %*% beta
# dRSS/dbeta = - 2 * t(X) %*% Y + 2 * t(X) %*% X %*% beta
# set dRSS/dbeta = 0

c( solve( t(x) %*% x ) %*% t(x) %*% y )



# 3) Gradient Descent

#RSS

# RSS = (Y - X %*% beta)' (Y - X %*% beta)
# RSS = t(Y) %*% Y - 2 * t(Y) %*% X %*% beta + t(beta) %*% t(X) %*% X %*% beta
# dRSS/dbeta = - 2 * t(X) %*% Y + 2 * t(X) %*% X %*% beta

# Gradient = -2 * t(x) %*% (Y - X %*% beta)

# w(t+1) = w(t) - alpha * gradient

w0 <- runif( 19 )

# w1 <- w0 - alpha * ( -2 * t(x) %*% (Y - X %*%  w0) )

# alpha = 0.0005

w1 = w0 + 0.0005 * 2 * t(x) %*% (y - x %*% w0)

while(sum(w1 == w0) != 19){
        
        w0 <- w1
        w1 <- w0 + 0.0005 * 2 * t(x) %*% (y - x %*% w0) 
}

c(w1)


#Example


x0 <- c(-3, -1)
gamma <- 0.085


x1 <- x0 -  gamma* c( 2*x0[1] + x0[2] - 5, x0[1] + 20*x0[2] - 3)


while( sum(x1 == x0) != 2){
        
        x0 <- x1
        
        x1 <- x0 - gamma * c( 2*x0[1] + x0[2] - 5, x0[1] + 20*x0[2] - 3)

        if(x0 == x1) break()

}

rm(x1, x0)









