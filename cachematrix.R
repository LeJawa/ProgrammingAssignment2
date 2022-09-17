## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix capable of keeping its inverse in cache memory.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function returns the inverse of a matrix, either by calculating it or by retrieving it from cache.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  
  if(!is.null(inverse)){
    return(inverse)
  }
  
  m <- x$get()
  inverse <- solve(m, ...)
  
  x$set_inverse(inverse)
  inverse
}

