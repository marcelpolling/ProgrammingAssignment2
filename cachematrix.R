## This function creates a special matrix which first sets the value, gets the value and then 
## sets and gets the inverse of a matrix

## it is assumed the input matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the matrix created with the above function. If the inverse is
## already calculates it will use the cached value

cacheSolve <- function(x, ...) {
        inv <- x$getInverse
        if(!is.null(invrs)) {
          message("getting cached data")
          return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat, ...)
        x$setinverse(invrs)
        invrs
}