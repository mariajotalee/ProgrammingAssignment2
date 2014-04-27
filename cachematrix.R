# General Comments about makeCacheMatrix and cacheSolve:
# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# cacheSolve computes the inverse of the special "matrix" object returned by makeCacheMatrix.
# If the inverse has been calculated, cacheSolve retrieves it from the cache.
# Example:
# > a <- matrix(runif(9), nrow=3, ncol=4)       //creates a matrix "a"
# > ca <- makeCacheMatrix(a)                    //creates a special matrix object "ca"
# > ca$get()                                    //returns the matrix object "ca"
# > cacheSolve(ca)                              //returns the inverse of the matrix "ca"
# > cacheSolve(ca)                              //matrix "ca" was already inverted above, therefore
#                                               //cacheSolve now returns "getting cached data"
#                                               //and retrieves the inverse from the cache

# makeCacheMatrix is a function that returns a list of
# functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <-function() m
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


# cacheSolve is a function that computes the inverse of the matrix
# unless it has been computed already.  If the inverse was computed
# before, it returns the cached inverse matrix.

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
