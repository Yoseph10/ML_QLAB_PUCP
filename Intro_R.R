

####### INTRODUCTION TO R

##################################
# 1)  BASIC COMMANDS
##################################

#--------------------------
# Concatenate function 
#--------------------------

y <- c( 2, 10 )

y

x <- c( 1 , 3 , 2 , 5 ) #numeric vector 


x = c( 1 , 6 , 2 )
x


y = c( 1 , 4 , 3 )

z = c( 5 , 2 , 5 , 9 )

y


#other types of vectors
v1 <- c(TRUE, FALSE, TRUE)
v2 <- c("Daniel", "Sandra", "Andrés")
v3 <- c(4, Daniel)
        

#---------------------------
#Length
#---------------------------

length( x ) 

length( y )

length( z )

x + y

x + z

?length #help



#---------------------------
#Ls and rm
#---------------------------

#Allows us to look at a list of all the objects, that we've saved
ls()

#Allow us to delete any that we don't want
rm( x , y )

ls()

#We can remove all objects at once. It's useful when working with a new script
rm( list = ls() )



#---------------------------
#Matrix
#---------------------------

?matrix

#Creating a 2 x 2 matrix
x <- matrix( data = c( 1, 2, 3, 4 ), nrow = 2,  ncol = 2 )
x

x <- matrix( c(1, 2, 3, 4) , 2 , 2 )


#Error
x <- matrix( c(1, 3, 4, 5) , 2 , 2 )


#By default R creates matrices by successively filling in columns
#The byrow = TRUE option can be used to populate the matrix in order of the rows
matrix( c(1 , 2, 3, 4) , 2 , 2 , byrow = TRUE)



#---------------------------
#Matrix Operations
#---------------------------

# A function that returns the square root of each element of a vector or matrix
x = sqrt( x )


# Raising each element of x to the power 2
x^3 


#---------------------------
#Random Numbers
#---------------------------

# This function generates a vector of random normal variables
x <- rnorm( 50 )

y <- x + rnorm( 50 , mean = 50, sd = .1 )


#It helps us to reproduce the exact same set of random numbers
set.seed( 10 )

x <- rnorm( 50 )

x <- x + rnorm( 50 , mean = 50, sd = .1 )

z <- y + rnorm( 50 , mean = 50 , sd = .1 )


set.seed( 3 )

var1 <- rnorm( 100 )

mean( var1 )    

var( var1 )

sqrt( var(var1) )

sd( var1 )



#---------------------------
#Correlations
#---------------------------

# Correlation between two variables
cor( x , y )


# Correlations between many variables

df <- data.frame(x, y, z)      #Creating a data frame

cor( df[ 3:5 , c("x",'y','z')] )

cor( df )

cor( df$x , df$z )



#---------------------------
#Sequences
#---------------------------

# The function seq() can be used to create a sequence of numbers
x <- seq( 5 , 20 )
x

#Short way to create a sequence
seq(1,10)

x <- 1:10
x

# Making a sequence of 10 numbers that are equally spaced
a <- seq( 0 , 1 , length = 10 )


x <- seq( -pi , pi , length = 50 )


##################################
# 2)  GRAPHICS
##################################


#---------------------------
#Basic plot
#---------------------------

?plot

set.seed( 15 )

x <- rnorm(100)
y <- rnorm(100)

#It produces a scatter plot
plot(x, y)

plot(x, y, xlab = "this is the x-axis",
     ylab = "this is the y-axis",
     main = "Plot of X vs Y")

plot(x, y, col = "red")

plot1 = plot(x, y, col = "red")


#setear nuestro directorio

#Saving plots
pdf( "plot1.pdf" )

jpeg( "plot1.jpeg" )

png( "plot1.png" )


#Indicates to R that we are done creating the plot
dev.off()


##################################
# 3) INDEXING DATA
##################################


A <- matrix( 1:16 , 4 , 4 )
A

#Selecting the element corresponding to the second row and the third column
A[2, 3]


#Selecting multiple rows and columns at a time
A[ c(1, 3) , c(2, 4) ]

A[1:3, 2:4]

#Include all columns
A[1:2, ]

#Include all rows
A[ , 1:2]

#R treats a single row or column of a matrix as a vector
A[1, ]

#The negative sign - keeps all rows or columns except those indicated in the index
A[-c(1, 3), ]

A[-c(1, 3), -c(1, 3, 4)]

#Outputs the number of rows and columns of a matrix
dim(A)


##################################
# 4) LOADING DATA
##################################


#setting your working directory (where your data is located)
setwd("C:/Q_lab/Taller_ML/PD1/data")


getwd()  


# Loading auto.data as data frame
Auto <- read.table("Auto.data")

is.data.frame(Auto)

View(Auto)

head(Auto)


Auto <- read.table("Auto.data", header = T, na.strings = "?", stringsAsFactors = T)

View(Auto)


#Loading a csv
Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = T)

View(Auto)

#number of rows (obs) and columns (variables)
dim(Auto)

Auto[1:4, ]

#missings
which( is.na(Auto$horsepower) ) #5 missings


print("Position of missing values by column wise")
sapply(Auto, function(x) which(is.na(x)))


#Remove rows with missings
Auto <- na.omit(Auto)

dim(Auto)

names(Auto)


##################################
# 5) ADDITIONAL GRAPHICAL AND NUMERICAL SUMMARIES
##################################


plot(cylinders, mpg)

#To refer a var, we must type the data set and the var name
plot(Auto$cylinders, Auto$mpg)

#Another alternative
attach(Auto)

plot(cylinders, mpg)


#Converting quantitative vars into qualitative vars
cylinders <- as.factor(cylinders)

#x-axis is qualitative -> boxplots
plot(cylinders, mpg)

plot(cylinders, mpg, col = "red")

plot(cylinders, mpg, col = "red", varwidth = T)

plot(cylinders, mpg, col = "red", varwidth = T,
     horizontal = T)

plot(cylinders, mpg, col = "red", varwidth = T,
     xlab = "cylinders", ylab = "MPG")

#Plotting a histogram
hist(mpg)

hist(mpg, col = 2) # col = red

hist(mpg, col = 2, breaks = 15)


#A scatterplot matrix 
pairs(Auto)

# Scatterplots for just a subset of the vars
pairs(
        ~ mpg + displacement + horsepower + weight + acceleration,
        data = Auto
)

#A numerical summary of each variable in a particular data set
summary(Auto)

#Producing a summary of just a single var
summary(Auto$mpg)

#quit
q()

