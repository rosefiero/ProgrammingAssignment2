## The makeCacheMatrix lets create a matrix, which can be viewed by get function, and its inverse created 
## by setInverse function. This function aslo allows to change the matrix using set(), while the getInverse 
## function returns the inverse if the setInverse() has been called earlier else remains NULL

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  setInverse <- function(){
    i <<- solve(x)
  }
  getInverse <- function(){
    i
  }
  
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function simply checks whether value of getInverse is NULL for a matrix created using
## makeCacheMatrix, if setInverse hasn't been called earlier, during which it computes the inverse else returns 
## previous value i.e. cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    m
}