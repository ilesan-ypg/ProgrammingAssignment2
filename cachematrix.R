## This is an example of a 'special' matrix object implementation, 
## in this case a matrix that can cache its own inverse for 
## efficiency.

## The function makeCacheMatrix is to create the 'special' matrix 
## object. It takes a regular matrix as its argument.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse with default NULL value
    inv <- NULL

    # The function that can be used to replace the input matrix with
    # another one.
    # NOTE: this function is not strictly necessary, since the matrix
    # is initialized when the "special" object is created (i.e. when
    # the main makeCacheMatrix() function is called).
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to return the stored value of the original, 
    # non-inverted matrix.
    get <- function() x
    
    # Function to set the inverted matrix.
    setinv <- function(i) inv <<- i
    
    # The function that returns the cached inverted matrix (or NULL
    # if one doesn't yet exist).
    getinv <- function() inv

    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve calculates the inverse of matrix cached 
## using makeCacheMatrix

cacheSolve <- function(x, ...) {
    
    # Check if a cached inverse doesn't exist, if not then create
    # the inverse and cache it.
    if(is.null(x$getinv())) x$setinv(solve(x$get()))
    
    # Return the cached result that is now guaranteed to exist.
    x$getinv()
}


## The function makeInvertibleMatrix is a helper function used to
## create a sample matrix that is guaranteed to be invertible, and
## thus ca be used to test the makeCacheMatrix and cacheSolve
## functions.
makeInvertibleMatrix <- function(n) {
    m <- matrix(1, n, n) 
    for(i in 1:n) {m[i,i] <- i}
    m
}