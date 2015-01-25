## The following code will take advantage of the scoping rules of the 
## Rlanguage to compute and cache the matrix'sinverse. there are 2 functions.

## This function creates a list that holds special functions to cache a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtrix <<- x;
    inverse <<- NULL;
  }
  get <- function() mtrix
  setinverse <- function(inverse) inverse <<- inverse;
  getinverse <- function() inverse
  list(set = set,
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the "special matrix"
## returned by te abve function "makeCacheMatrix". If the inverse has
## already been calculated and the matrix has not changed, then
## "cacheSolve" will return the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- mtrix$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtrix$get()
  invserse <- solve(data, ...)
  mtrix$setinverse(inverse)
  inverse
}
