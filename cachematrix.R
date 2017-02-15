## Put comments here that give an overall description of what your
## functions do

## This function describes further sub funtions
##It sets, retrieves inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## This function calculates the inverse of the sqaure matrix
## it takes matrix as an input

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    message("Getting Cached Data")
    return(inverse)
  }
  
  
  mat <- x$get()
  
  inverse <- solve(mat, ...)
  
  x$setInverse(inverse)
  
  inverse
  
}
