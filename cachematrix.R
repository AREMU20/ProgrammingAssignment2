## Put comments here that give an overall description of what your
## functions do
## These functions are used to cache the inverse of a matrix to avoid 
## repeated and potentially time-consuming computations.
##
## makeCacheMatrix creates a special matrix object that can store a matrix 
## and cache its inverse.
##
## cacheSolve computes the inverse of the special matrix created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), it retrieves the cached inverse instead of 
## recomputing it.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear the previously cached inverse
}
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Return a list of the above functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
        ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
}
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse
  x$setinverse(inv)       # Cache the inverse
  inv                     # Return the result
}