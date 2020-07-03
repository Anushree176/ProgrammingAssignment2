## Since matrix inversion is a costly computation, thus we cache 
## the inverse of the matrix to avoid repetative calculation

## Has special matrix that caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(invertMatrix) inverse <<- invertMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computation of inverse of the matrix returned by the above function
## if not calculated, or else return cache value
## solve() - computes inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Getting cached data...")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
}