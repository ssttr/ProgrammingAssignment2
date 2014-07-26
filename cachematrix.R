## #############################################################
##
## The makeCacheMatrix function extends a mtrix object with  two attributes anfd 5 methods
##
##  attribute x = matrix
##  attribute m = Inverse of matrix
##
##  Method makeCacheMatrix(matrix)    # inititator set x = matrix and m = null
##  method set (matrix)               # set x = matrix and m = null
##  method get()                      # get x
##  method setInverse()               # compute m
##  method getInverse()               # get m


makeCacheMatrix <- function(x = matrix()) 
{
    
    m <- NULL
    
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function() m <<- solve(x)
    { 
        m 
    }
    
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  
    
}

## #############################################################
## The cacheSolve function utilizes the makeCacheMatrix objext to not recompute 
## the inverse of a matrix when it has been previously computed
## it has no attribues and its only methof is the initiator.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the Inverse of 'x'

    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    m <- x$setInverse()
    m

    
}


## #############################################################
# sample test code

#  a<- matrix(1:4, 2 ,2)
#  b <- makeCacheMatrix(a)
#  
#  b$get()
#  b$getInverse()
#  cacheSolve(b)
#  b$get()
#  b$getInverse()