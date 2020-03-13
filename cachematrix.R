## These functions allow us to save the values of inverses of
## matrices that have been previously computed.

## This function returns a list of 4 functions that are needed for
## the second function.

makeCacheMatrix <- function(x = matrix()) {
    # The argument x is the matrix we want to invert.
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # This allows us to set a new value to the matrix x.
    
    get <- function() x
    # Gives us the value of matrix x.
    
    setinv <- function(found) inv <<- found
    # This will allows us to save the value of the inverse,
    # once it's computed for the first time.
    
    getinv <- function() inv
    # Gives us the value of inv, which is either NULL (if the
    # inverse matrix hasn't been calculated yet) or it is the
    # value of the inverse matrix (once it has been found and 
    # saved here).
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function makes use of the first function to either compute
## the inverse of a matrix for the first time and save it, or to  
## take the previously saved value of the inverse.

cacheSolve <- function(x, ...) {
    ## The object x must be the list returned by the first function
    ## on the matrix we want to invert.
    
    inv <- x$getinv()
    
    # If the inverse has been calculated before
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If the inverse hasn't been calculated yet
    data <- x$get()
    inv <- solve(data, ...)     # we calculate it
    x$setinv(inv)               # and we save it
    inv
}
