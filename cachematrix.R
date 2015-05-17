## These functions provide the ability to use a cached version of an inverse to a given matrix.

# makeCacheMatrix creates a matrix of a given vecotr x. During generation or modification, the inverse of the
# matrix set to null. There are functions for setting and getting the reverse of the matrix via call to the
# function solve. The caching of the inverse is triggered by another function cacheSolve.
makeCacheMatrix <- function(x = numeric()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function returns the inverse matrix of a given matrix x. It provides caching functionality by using
# cached values for the inverse if these exist and by calculating and storing the inverse otherwise. The
# inverse is cached within the funtion makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  } else {
    message("calculating fresh data")
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
  }
}