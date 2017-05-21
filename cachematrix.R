## This script calculates the inverse of a matrix and caches the 
## inverse for later use.

## Input the matrix to calculate the inverse of. This function
## also creates a list of functions needed for the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y){
    inv <<- NULL
    x <<- y
  }
  getmatrix <- function(){x}
  setinv <- function(inverse){
    inv <<- inverse
  }
  getinv <- function(){inv}
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv, getinv = getinv)
}

## Calculates the inverse of the requested matrix and caches it.
## If the inverse is already cached then the cached inverse is returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv) == TRUE){
    message("Getting cached inverse.")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
