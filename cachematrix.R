## makeCacheMatrix and cacheSolve functions computes inverse of a matrix and caches it

## makeCacheMatrix() function creates an R object that can store (cache) a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() function calculates the inverse of the matrix that is returned by makeCacheMatrix().
## cacheSolve() function retrieves the cached inverse from the cache if the inverse was already
## calculated.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
