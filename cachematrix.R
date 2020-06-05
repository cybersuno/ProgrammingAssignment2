## Following functions are responsible to calculate the inverse of a matrix
## with the possibility of using a cache when original matrix is not changed
## To do so, a pair of functions are created, aimed to create a 
## "cacheable inverse" matrix and calculate (and reuse) the inversion

## makeCacheMatrix:
## arguments: x --> an invertible matrix
## result: results is a list of functions, in the form of an instance of a
## cacheable matrix. The funcions are:
## get --> getter for the matrix. Returns the matrix
## set(y) --> setter for the matrix. Stablishes a new matrix (and erases cache)
## getInverse --> returns the cached inverse matrix
## setInverse(im) --> stablishes the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  #variables:
  #x --> internal matrix
  #cachedInverse --> inverse for x, if it has been calculated (cache)
  
  #set to NULL the cached inverse matrix
  cachedInverse <- NULL
  
  #setter method: set the x in the function scope and "erases" cache
  setX <- function(newMatrix) {
    x <<- newMatrix
    cachedInverse <<- NULL #erase cache
  }
  
  #getter for the matrix
  getX <- function() {
    x
  }
  
  #setter for the cached inverse matrix
  setInverse <- function(inverse) {
    cachedInverse <<- inverse #sets the function scope cachedInverse with the argument
  }
  
  #getter for the cached inverse matrix
  getInverse <- function() {
    cachedInverse
  }
  
  #returns the "method list"
  list(setX = setX, getX = getX,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve:
## arguments: x --> a CacheMatrix special matrix
## result: results the inverse of the matrix, but if it is cached, it is not 
## calculated again, but retrieved from cache
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #sets inverse with the cached inverse matrix. If is not cached, a NULL is got
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("Cached inverse matrix")
    return(inverse)
  }
  matrix <- x$getX()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
