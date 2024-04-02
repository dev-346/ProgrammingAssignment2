# Function to create a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache for storing the inverse matrix
  cache <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    cache <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    cache
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of a cached matrix
cacheSolve <- function(x, ...) {
  # Get the cached inverse if available
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Retrieving cached inverse")
    return(inverse)
  }
  
  # If inverse is not cached, compute the inverse
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  
  # Cache the inverse
  x$setInverse(inverse)
  
  # Return the inverse
  inverse
}

# Example usage:
# Create a special matrix object
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

# Compute the inverse of the matrix
cacheSolve(my_matrix)
