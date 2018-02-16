## Functions below create an object to store a matrix and cache its inverse.

## This functions creates a "matrix", which is really a list of functions to set and get the value of the matrix, 
## and to set and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematr <- function(inversematr) m <<- inversematr
  getinversematr <- function() m
  list(set = set, get = get,
       setinversematr = setinversematr,
       getinversematr = getinversematr)
}

## This function calculates the inverse of the "matrix", returned by the makeCacheMatrix, if the inverse hasn't been calculated before,
## or skips calculation and gets the inverse from the cache, if it already has been calculated.

cacheSolve <- function(x, ...) {
  m <- x$getinversematr()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematr(m)
  m
}
