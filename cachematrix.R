## Functions to create a special matrix object that can cache its inverse
## and a solver that makes use of this cache

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(get = get, set = set,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of the "cacheable matrix" and cache it
## or returns inverse from the cache if exists
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
