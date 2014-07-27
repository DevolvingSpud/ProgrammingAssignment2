## These functions provide a way to cache the inverse of
## a matrix.  Example of use:
##
##
## m <- matrix(1:4,2,2)
## mc <- makeCacheMatrix(m)
## cacheSolve(mc)  # will calculate the inverse
## cacheSolve(mc)  # will use the cached inverse

## makeCacheMatrix
##
## Creates a matrix wrapper that lazily computes
## and caches the inverse.
makeCacheMatrix <- function(x = matrix()) {

    # Sets the inverse to null.  We don't calculate it
    # until we need to.
    i <- NULL
    
    # Function to (re)set the matrix.  When you do
    # this, the inverse is re-nullified.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse to a value
    setInverse <- function(inverse) i <<- inverse
    
    # Function to get the inverse
    getInverse <- function() i
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## cacheSolve
##
## Computes the inverse of a cacheMatrix
## and stores it appropriately.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    # Return the cached value if there is one.
    if (!is.null(inverse)) {
        message("Using cached value")
        return(x$getInverse())
    }
    
    # Otherwise, we have to compute the inverse.
    # Retrieve the matrix.  We assume invertible per
    # instructions.
    y <- x$get()
    
    # Ensure it is not null and that it is a matrix.
    if(!is.null(y) && is.matrix(y)) {
        message("Computing inverse")
        inverse <- (solve(y))
        x$setInverse(inverse)
        return(x$getInverse())
    }
    NULL
}
