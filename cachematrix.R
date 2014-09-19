## The function pair 'makeCacheMatrix' and 'cacheSolve' takes
## advantage of the scoping roles of R to cache the value of a matrix's
## inverse so it can be retrieved without computation if the matrix
## contents have changed

## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    ## Returns a list containing 4 functions to
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse
    ## 4. get the value of the inverse
}


## 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'
## If the inverse has already been calculated (and the matrix has not changed), 
## then 'cacheSolve' retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv    ## Return a matrix that is the inverse of 'x'
}
