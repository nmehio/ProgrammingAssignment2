#This function utilizes a cache to reduce the cost of inverting a matrix.

#This function creates a special matrix object that can cache its inverse matrix.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  storeMatrix <- function(newValue){
    x <<- newValue
    m <<- NULL
  }
  returnMatrix <- function(){
    x
  }
  invertMatrix <- function(solve){
    m <<- solve
  }
  returnInverse <- function(){
    m
  }
  list(storeMatrix = storeMatrix, returnMatrix = returnMatrix, invertMatrx = invertMatrix, returnInverse = returnInverse)
}

#This function computes the inverse matrix if it has not already been computed.  
#If it has already been computed, then it retrieves the value of the inverse matrix from the cache.
cacheSolve <- function(x, ...){
  m <- x$returnInverse()
  if(!is.null(m)){
    message("Retrieving cached data.")
    return(m)
  }
  data <- x$returnMatrix()
  m <- solve(data)
  x$invertMatrix(m)
  m
}
