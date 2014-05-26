##################################################
##
## makeCacheMatrix() and cacheSolve()
## - pair of functions that cache the inverse of a matrix
## - v1.0, 2014-05-17
##
## <<- operator assigns a value to an object in an environment different from the current environment
##  - can be thought of as a global variable
##

##########
## makeCacheMatrix
## - creates a special "matrix" object that can cache its inverse
## - consists of a list containing functions to
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse
##  - get the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##########
## cacheSolve
## - computes the inverse of the special "matrix" returned by makeCacheMatrix and cache it
## - first checks to see if the inverse has already been calculated
## - if inverse already calculated (and matrix unchanged), then retrieve the inverse from the cache
## - otherwise, it calculates the inverse of the data and caches it via the setInverse function
##
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
