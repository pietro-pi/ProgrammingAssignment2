## Implement a result-caching system
## Adapted from the example code provided

## makeCacheMatrix takes a matrix as input and sets the cached result to NULL
## provide get/set functions
## provide cached-value accessors

makeCacheMatrix <- function(x = matrix()) {
     mcache <- NULL
     set <- function(y) {
          ## when setting or changing the matrix, clear the (possibly invalidated) cache
          x <<- y
          mcache <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) mcache <<- inverse
     getinverse <- function() mcache
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, env_ls = ls())
}


## cacheSolve, when given a list from makeCacheMatrix, returns the inverse of the underlying matrix
## when possible, return the cached value instead of calculating it
## if we have to calculate it, then cache the calculated value to save time later

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     minv <- x$getinverse()
     if(!is.null(minv)) {
          message("getting cached inverse")
          return(minv)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
