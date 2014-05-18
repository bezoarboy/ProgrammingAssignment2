## makeCacheMatrix() and cacheSolve()
## - pair of functions that cache the inverse of a matrix

##########
## makeCacheMatrix
## - creates a special "matrix" object that can cache its inverse
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
## - return a matrix that is the inverse of 'x'
## - computes the inverse of the special "matrix" returned by makeCacheMatrix and cache it
## - if inverse already calculated (and matrix unchanged), then retrieve the inverse from the cache
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
