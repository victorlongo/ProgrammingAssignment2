## makeCacheMatrix() and cacheSolve() are a pair of functions 
## that cache the inverse of a matrix.


## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) i <<- invert
  getinvert <- function() i
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## cacheSolve() computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  i
}