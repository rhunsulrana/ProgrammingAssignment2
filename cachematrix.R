## @author Brendan Swiniarski 2014-07-21 - from stub provided in 
## Coursera course rprog-005

## This pair of functions allows us to take advantage of R's environmental
## scoping and cache the inverse of a matrix, using a helper 'class' 
## called makeCacheMatrix.

## This creates and returns an 'object'-like list of functions
## including functions to set and get the matrix this 'object' represents
## as well as getting and setting the inverse of this matrix.
## This list is designed to work with the cachSolve function below.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    m <<- x
    
    set <- function(newMatrix) {
        m <<- newMatrix
        inv <- NULL
    }
    
    get <- function() {
        m
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This will return the inverse of the matrix. If a cached version exists
## in the environment, that is what is returned, otherwise it is calculated
## and returned using the solve() function, and also cached for future needs.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## will return cached inverse from the environment if it exists
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse.")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinvers(inv)
    inv
}
