## These functions make cache of the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inV <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInV <- function(inverse) inV <<- inverse
  getInV <- function() inV
  list(set = set, get = get, setInV = setInV, getInV = getInV)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inV <- x$getInV()
  if(!is.null(inV)) {
    message("getting cached data")
    return(inV)
  }
  data <- x$get()
  inV <- solve(data, ...)
  x$setInV(inV)
  inV
}
