#This function utilizes a cache to reduce the cost of inverting a matrix.

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
