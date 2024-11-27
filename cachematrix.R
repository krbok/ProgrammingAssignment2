## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of all functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }
  



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Check if inverse is already cached
  inv <- x$getInverse()
  
  # If cached, retrieve it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
