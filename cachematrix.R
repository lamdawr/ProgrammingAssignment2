## Lakshmi Muralidharan
## 5/24/2014


## The functions written below calculates inverse of a matrix and stores it
## in cache leveraging lexical scoping so that the time consuming process 
## can be made simpler by accessing the values from the cache.

## The function given below calculates the inverse of the matrix and is
## used to access the matrix.'<<-' is used to store in a different environment

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


## This function will find the inverse of the matrix defined above but will 
## first check the cache for values

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
