#This function utilizes a cache to reduce the cost of inverting a matrix.

#This function creates a special matrix object that can cache its inverse matrix.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  setMatrix <- function(newValue){
    x <<- newValue
    m <<- NULL
  }
  getMatrix <- function(){
    x
  }
  setInverse <- function(solve){
    m <<- solve
  }
  getInverse <- function(){
    m
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

#This function computes the inverse matrix if it has not already been computed.  
#If it has already been computed, then it retrieves the value of the inverse matrix from the cache.
cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("Retrieving cached data.")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m)
  m
}
