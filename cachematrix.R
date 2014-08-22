## Sylvester Willis
## These functions enable the caching of the inverse of a matrix that is given.

## This is an object which stores the result 
## of a matrix inverse operation.
makeCacheMatrix <- function(x = numeric()) {
  if(nrow(x) != ncol(x)) {
    stop("matrix x must be square")
  }
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) s <<- inverse
  getInverse <- function() s
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function calls the matrix inverse 
## function if the result is not already stored.
cacheSolve <- function(x, ...) {
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
}
