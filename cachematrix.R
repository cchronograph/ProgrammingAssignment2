## a pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() {
    inv
  }
  
  list(myset = set, myget = get,
       mysetinverse = setinverse,
       mygetinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  inv <- x$mygetinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$myget()
  inv <- solve(data, ...)
  x$mysetinverse(inv)
  inv
}
