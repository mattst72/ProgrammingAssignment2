## These two functions save computation time by caching the value of the
## inverse of a matrix. The function makeCacheMatrix creates a special "matrix"
## object that cache its inverse, and the function cacheSolve checks to see if
## the inverse of a matrix has already been calculated before calling the
## solve() function to compute it from scratch.

## The function makeCacheMatrix sets the value of the matrix, gets the value of 
## the matrix, sets the value of the inverse, and gets the value of the inverse. 
## The inverse of the matrix is computed via the solve() function.
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The function cacheSolve receives as its argument makeCacheMatrix(A),
## for some invertible matrix A. If A^(-1) has already been computed, it gets 
## the cached value. If it has not already been computed, it computes A^(-1) via
## the solve() function.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
