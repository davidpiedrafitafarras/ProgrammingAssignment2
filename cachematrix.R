## Set of functions which allow to cache the inverse of a given matrix.
## The functions should be used in the following order:
##      1) makeCacheMatrix
##      2) cacheSolve
## See each function for usage instructions
## Please note that tHe matrix is assumed to be invertible, so the proper
## checks should be codified by the user, if deemed necessary 

## makeCacheMatrix
## Description: takes a regular matrix and returns a cacheable "matrix" to be 
##          used with the function cacheSolve
## Usage: makeCacheMatrix(a)
## Arguments: a -- square matrix (which should be invertible)

makeCacheMatrix <- function(x = matrix()) {

    # Initalize inverse to null
    
    inverse <- NULL 
    
    # If the matrix is redefined, set function assigns the argument to x and set
    # the inverse to null as the old value is no longer valid and should be
    # recalculated
    
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    
    # get function returns the original matrix
    get <- function() x
    
    #setinverse stores the result of inverting the matrix
    setInverse <- function(solve) inverse <<- solve
    
    #getInverse returns the value of the inverted matrix
    getInverse <- function() inverse
    
    #list of functions created
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve
## Description: takes the matrix created by makeCacheMatrix and returns its
##          inverse. If this value has been calculated before, the function
##          returns the cached value.
## Usage: cacheSolve(a,...)
## Arguments: a -- Special Matrix created with makeCacheMatrix
##            ... further arguments for the solve function

cacheSolve <- function(x, ...) {
    
    # checks if x is formally a correct argument 
    # ( a list with the appropiate functions )
    
    if(!is.list(x)) {
        message("Non suitable argument. Please use makeCacheMatrix")
        return()        
    } else if(!(is.function(x$getInverse) & is.function(x$setInverse)
         &is.function(x$set) & is.function(x$get))) {
        message("Non suitable argument. Please use makeCacheMatrix")
        return()         
    }
    
    # Take the current value of the inverse, and if it's not null, returns 
    # the cached inverse matrix. In the other case, the function continues
    
    invMat <- x$getInverse()
    
    if(!is.null(invMat)) {
        message("Getting cached inverse matrix")
        return(invMat)
    }
    
    # Get the value of the values of the matrix 
    
    data <- x$get()
    
    # Uses solve function to invert the matrix
    
    invMat <- solve(data,...)
    
    # Set the value of the inverse in order to be cached the next time 
    
    x$setInverse(invMat)
    
    # Print the value of the inverse matrix into the command line
    invMat
}
