## This is an example of a 'special' matrix object implementation, in this case
## a matrix that can cache its own inverse.

## The function makeCacheMatrix is to create a 'special' matrix object
## that will be able to cache within itself its own inverse for 
## efficiency

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(new.inv) inv <<- new.inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse of matrix cached 
## using the function makeCacheMatrix()

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached inverse...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
    
}
