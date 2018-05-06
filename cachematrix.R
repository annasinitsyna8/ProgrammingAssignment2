## function 'makeCacheMatrix' creates the object, which provides the list of operations
## function 'cacheSolve' returns the inverse matrix from cache if exists or calculates it otherwise
## function 'cacheSolve' takes an object, which is created by the 'makeCacheMatrix'

## Creates environment for matrix 'x' and provides the list of operations:
## set - sets the x value and resets inverse
## get - retrieves the matrix x
## setInverse - sets the inverse value
## getInverse - retrieves the inverse value of matrix x

makeCacheMatrix <- function(x = matrix()) {

  inverseX <- NULL
  set <- function(y){
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseX <<- solve
  getInverse <- function() inverseX
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Returns a matrix that is the inverse of the square invertible matrix 'x'
## from cache if the value exists in cache, otherwise calculates, caches and returns the result

cacheSolve <- function(x, ...) {
  
  inverseX <- x$getInverse()
  if (!is.null(inverseX)){
    message("getting cache data")
    return(inverseX)
  }
  
  data <- x$get()
  message("calculating and caching data")
  inverseX <- solve(data, ...)
  x$setInverse(inverseX)
  inverseX
  
}
