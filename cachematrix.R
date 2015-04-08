## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Belowe there are two functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to
## 
## 1.  set the value of the maxtix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(current_matrix = matrix()) {
    inverse_matrix <- NULL
    set_matrix <- function(new_matrix) {
        current_matrix <<- new_matrix
        inverse_matrix <<- NULL
    }
    get_matrix <- function() current_matrix
    set_inverse <- function(new_inverse) inverse_matrix <<- new_inverse
    get_inverse <- function() inverse_matrix
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve calculates the inverse of the special "matrix" created 
## with makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `set_inverse`
## function.

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    current_matrix <- x$get_matrix()
    inverse_matrix <- solve(current_matrix, ...)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
