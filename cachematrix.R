## makeCacheMatrix and cacheSolve, together, take an input matrix x,
## return its inverse, and save that inverse to cache.
##


## The makeCacheMatrix function returns a list of functions
## used to get a square matrix x, calculate it's inverse,
## and save the result in cache.
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns the inverse of x from makeCacheMatrix if
## already calculated. If not, it calculates the inverse of x
## and sets the inverse in the cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
