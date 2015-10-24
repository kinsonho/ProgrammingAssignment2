## To calculate the inverse of a matrix and caches the resulting inverse.
## If the same matrix is used, calling the cacheSolve() on the same
## matrix will return the cached inverse.

## makeCacheMatrix takes a matrix and returns a list with four functions:
## set(m) - set the matrix to the cache. Inverse is set to NULL
## get() - get the cached matrix
## setInv(mInv) - set the cache inverse value to have the value of mInv
## getInv() - get the cache inverse value

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        get <- function() x
        setInv <- function(m) mInv <<- m
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve calculates the inverse of a matrix and caches the result. The
## matrix must be created using the makeCacheMatrix() function.
## The calculated inverse is cached so that subsequent call to cacheSolve()
## with the same wrapped matrix will return the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInv()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data, ...)
        x$setInv(mInv)
        mInv
}
