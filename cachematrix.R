## Since the computation of a matrix inversion can be time-consuming (even in R), it makes sense to 
## cache the results for given matrices so that their inverted form can be returned immediately.
## In the formulas below, makeCacheMatrix returns a list of formulas that cacheSolve uses to check if 
## a matrix has been inverted before, in which case the result can be returned instantly.

## makeCacheMatrix contains the following functions:
# - 'set' the value of the matrix
# - 'get' the value of the matrix
# - 'setinv' to set the inversion result for the matrix
# - 'getinv' to get said inversion result for the matrix
makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
  x <<- y
  i <<- NULL
}
get <- function() x
setinv <- function(inv) i <<- inv
getinv <- function() i
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## When passed a matrix, cacheSolve is 
# - using x$getinv() to get the inversion result currently stored for a given matrix in makeCacheMatrix
# - returning the result if there is one
# - if there is no result for x, cacheSolve calculates the inversion and stores it in makeCacheMatrix
#   (this way future computations can be sped up)
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
