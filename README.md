# programmingassignment2
### Programming Assignment 2 for Coursera

makeCacheMatrix <- function(x = numeric()) {
  
  # holds the cached value or NULL if nothing is cached
  # At first, no item is cached so it is set to NULL
  cache <- NULL
  
  # Storing the Matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # As the matrix is given a new value, the cache has to be flushed
    cache <<- NULL
  }
  
  # The Return of the Stored Matrix
  getMatrix <- function() {
    x
  }
  
  # Cache the Given Argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # Get the Cached Value
  getInverse <- function() {
    cache
  }
  
  # The Return of List. Each named element of the list down below is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with makeCacheMatrix

cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # If not, we need the matrix, caclulate the inverse and store it in the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # Return of the Inverse
  inverse
}
