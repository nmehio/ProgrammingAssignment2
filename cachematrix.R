#This function utilizes a cache to reduce the cost of inverting a matrix.

#This function creates a special matrix object that can cache its inverse matrix.
makeCacheMatrix <- function(x = matrix()){
  #This variable stores the cached matrix or NULL if nothing is stored in the cache.
  m <- NULL
  #This functon stores the new value of the matrix.
  #This function also clears the cache because the matrix has changed.
  setMatrix <- function(y){
    x <<- y
    m <<- NULL
  }
  #This function returns the stored matrix.
  getMatrix <- function(){
    x
  }
  #This function inverts the matrix and stores it in the cache.
  setInverse <- function(solve){
    m <<- solve
  }
  #This function returns the cached inverse matrix.
  getInverse <- function(){
    m
  }
  #This named list contains all the functions used in the makeCacheMatrix function.
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

#This function computes the inverse matrix if it has not already been computed.  
#If it has already been computed, then it retrieves the value of the inverse matrix from the cache.
cacheSolve <- function(x, ...){
  #This line of code retrieves the cached inverse matrix.
  m <- x$getInverse()
  #This function returns the cached inverse matrix if it exists.
  if(!is.null(m)){
    message("Retrieving cached data.")
    return(m)
  }
  #This block of code computes the inverse matrix is not stored in the cache.
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m)
  m
}
