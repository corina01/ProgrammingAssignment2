## Functions makeCacheMatrix and cacheSolve create a matrix then compute the
## inverse of the matrix if it has not already been calculated.

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversion <- function(solvematrix) m <<- solvematrix
        getinversion <- function() m
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinversion()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversion(m)
        m
}
