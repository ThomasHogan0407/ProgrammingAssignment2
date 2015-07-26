
## The paired functions below cache and compute the inverse of a
## matrix.

## The function 'makeCacheMatrix' creates a matrix object that will 
## cache its inverse

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = 		getinv))
}

## The function 'cacheSolve' computes the inverse of the special 
## "matrix" which is returned by 'makeCacheMatrix'.
## If the inverse has already been calculated, 'cacheSolve' will 
## retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}