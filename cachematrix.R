# Week 3 Programming Assignment 2: Lexical Scoping ----

### Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  # Initialise the inverse property and set the matrix
  i <- NULL
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  # Retrieve then return the matrix
  get <- function(){
    m
  }
  # Set the inverse of the matrix
  setInverse <- function(inverse){
    i <<- inverse
  }
  # Get the inverse of the matrix then return it
  getInverse <- function(){
    i
  }
  # Finally, return a list of the methods
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  # Return the inverse only
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  # Get the matrix from our object and calculate the inverse
  data <- x$get()
  m <- solve(data) %*% data
  # Set the inverse to the object and return the matrix
  x$setInverse(m)
  m
}
