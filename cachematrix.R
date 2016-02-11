
## Two functions that provide matrices with the capability to cache their
## reverse, thereby avoiding its recalculation

## A special caching matrix is created by applying this function to a regular
## R matrix

makeCacheMatrix <- function(x = matrix()) {
    reverse <- NULL
    list(
        set = function (y = matrix()) {
            x <<- y
            reverse <<- NULL
        },
        get = function () { x },
        setRev = function (y) { reverse <<- y },
        getRev = function () { reverse }
    )
}


## This function reverses a special caching matrix.  If the reverse is
## already cached, it does not perform any computation.
## Precondition: It is assumed that the matrix is invertible.

cacheSolve <- function(x, ...) {
    reverse <- x$getRev()
    if (is.null(reverse)) {
        reverse <- solve(x$get(), ...)
        x$setRev(reverse)
    }
    reverse
}
