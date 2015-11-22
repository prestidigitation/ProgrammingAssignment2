## Function that creates a matrix object that can cache its inversion.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Function that calculates the inverse of a matrix object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inversion <- x$get_inverse()
        if (!is.null(inversion)) {
                message("Retrieving cached data...")
                return(inversion)
        }
        matrix <- x$get()
        inversion <- solve(matrix, ...)
        x$set_inverse(inversion)
        inversion
}
