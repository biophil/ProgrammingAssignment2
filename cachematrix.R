## These two functions are designed to facilitate caching the inverse of a matrix.
## The first function creates an object that stores a matrix and its inverse,
##      along with helper functions to read and write the matrix and its inverse.
#
## The second function is meant to exactly emulate the behavior of the built-in solve()
##      function, except that it first checks if the matrix inverse has already been
##      calculated. 



## The makeCacheMatrix() function creates a list object that stores a matrix and its 
##      inverse and set/get functions for these two quantities.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solution) inv <<- solution
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}



## The cacheSolve() function returns the inverse of a matrix that is stored in the
##      cached format created by the makeCacheMatrix() function.
#
## cacheSolve(x) will return the exact same output as solve(x$get()); the only difference
##      is that cacheSolve(x) checks x$getInv() first to see if the inverse has already
##      been computed, in which case the cached inverse is returned.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
