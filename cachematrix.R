

## Comments shall give the user an idea of what each function does. 

### This script has two functions: one that stores a matrix and its inverse and another that is used to populate or get the inverse from an object of makeCacheMatrix().

## The makeCacheMatrix() function takes the default input, x, an invertible matrix. We then program the inverse of a matrix to NULL.
## Afterward, we shall use the set() function that takes an argument y, an invertible matrix itself, and assigns it to the x variable in the parent environment.
## The (<<-) assignment operator shall be used as a result.
## The function shall also assign a NULL value to the inverse variable.
## The get() function subsequently receives the x value.
## As we look at the setInverse function, as the inv variable is defined in the parent function, it can only be accessed via the (<<-) assignment operator.
## The getInverse function shall receive the inverse value of the matrix.
## The final part of the function assigns each function as an element of a list and then goes back to the parent environment.
## The naming convention can help using the $ operator to extract.

## All known objects within makeCacheMatrix() are able to access all the functions get(), set(), setInverse(), and getInverse()

makeCacheMatrix <- function(x = matrix()) {
  nva <- NULL                                        
  ## This is to have the inverse be set to NULL
  
  ## The set function can also reset the value of the matrix with a new value. 
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x                                ## This shall obtain the value of matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() nva                       # This shall obtain the value of the inverse of matrix x
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


## The get function basically obtains the inverse via the getInverse() function. Then checks if it is NULL, the inverse shall be calculated.
## If it is not a NULL value, a cached inverse value is computed. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  nva <- x$getInverse()                              # This shall try to obtain the inverse of the matrix through getInverse() function.
  if (!is.null(nva)){                                # This shall check if the given inverse is NULL. If the result of the condition (!is.null(inv)) is TRUE, the cached inverse is then displayed.
    message("Getting Cached Data")
    return(nva)
  }                                                  # If the of the condition (!is.null(inv)) is FALSE, the inverse is solved and it returns as inv.
  matr <- x$get()
  nva <- solve(matr,...)
  x$setInverse(nva)
  nva
  
}
