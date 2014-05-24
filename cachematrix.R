# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

makeCacheMatrix <- function(x = matrix()) {
    bat <- NULL
    lam <- function(y) {
        x <<- y
        bat <<- NULL
    }
    bar <- function() x
    laminverse <- function(inverse) bat <<- inverse
    barinverse <- function() bat
    list(lam=lam, bar=bar, laminverse=laminverse, barinverse=barinverse)
}




# The function gives the inverse of the matrix. It first checks if
# the inverse has already been calculated. If so, it gets the result and skips the
# computation. If not, it calculates the inverse, sets the value in the cache via
# batinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    bat<- x$barinverse()
    if(!is.null(bat)) {
        message("getting cached data.")
        return(bat)
    }
    data <- x$bar()
    bat <- solve(data)
    x$laminverse(bat)
    bat
}
