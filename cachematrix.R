## This file contais two functions for 'cached' inverse matrix calculation:
##   - makeCacheMatrix: This function creates a special "matrix" object that can
##                       cache its inverse.
##   - cacheSolve: This function computes the inverse of the special "matrix" 
##                returned by makeCacheMatrix above. If the inverse has already 
##                been calculated (and the matrix has not changed), then 
##                cacheSolve should retrieve theinverse from the cache.


# This function creates a special "matrix" object that can cache its inverse.
#
# Args:
#   x: Matrix
#
# Returns:
#  list of functions (set, get, setinv, getinv)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}



# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from
# the cache.
#
# Args:
#   x: Cache Matrix (see makeCacheMatrix)
#
# Returns:
#   Inverse of given matrix 'x'

cacheSolve <- function(x, ...) {
  ## Get 'cached' inverse of 'x' and return value if already precomputed
  m <- x$getinv()
  if(!is.null(m)) {
    return(m)
  }
  
  ## If the 'cached' inverse was not previously precomputed:
  ##   - calculate the inverse (with solve)
  ##   - cache the inverse
  matrix_data <- x$get()
  m <- solve(matrix_data)
  x$setinv(m)
  
  ## Return the inverse of 'x'
  m  
}
