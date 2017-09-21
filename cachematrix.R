# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than computing it repeatedly. 
# this code has a pair of functions that cache the inverse of a matrix.

# the function makeCacheMatrix creates a special "matrix" which is actually a list 
# containing a function to set the value of the matrix, get the value of a matrix, 
# set the value of the inverse of the matrix, get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) mat_inverse <<- inv
  getinverse <- function() mat_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The function cacheSolve calculates the inverse of the special "matrix" created with the 
# above function. However, it first checks to see if the inverse has already been 
# calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
# of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  mat_inverse <- x$getinverse()
  if(!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data_matrix <- x$get()
  mat_inverse <- solve(data_matrix, ...)
  x$setinverse(mat_inverse)
  mat_inverse
        ## Return a matrix that is the inverse of 'x'
}
