## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly 

## making a special list containing a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)


}


## making the inverse of matrix and caching it to previous list but if it found it
## in the previous list it calls it without any calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data^-1, ...)
    x$set_inv(inv)
    inv
}
