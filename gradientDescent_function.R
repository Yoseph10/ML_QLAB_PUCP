GD <- function(x0, gamma) {
        'x0 needs to be vector of length of 2'
        'gamma needs to be a number'
        
        
        x1 <- x0 -  gamma* c( 2*x0[1] + x0[2] - 5, x0[1] + 20*x0[2] - 3)
        
        
        while( sum(x1 == x0) != 2){
                
                x0 <- x1
                #print(x0)
                
                x1 <- x0 - gamma * c( 2*x0[1] + x0[2] - 5, x0[1] + 20*x0[2] - 3)
                
                ifelse(x0 == x1, break, next)
                
        }
        
        
        return(x1)
}


#example

x0 <- c(-3, -1)
gamma <- 0.085

GD(x0, gamma)