## Calculating the inverse of a matrix can be time consuming.
## Use the following functions to create and calculate the inverse of a matrix.

# EXAMPLE usage
# Note that calling cacheSolve the second time returns cached data
#
# m <- matrix(c(4,3,3,2), nrow = 2, ncol = 2, byrow=TRUE)
# l <- makeCacheMatrix(m)
# cacheSolve(l)
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
#
# cacheSolve(l)
# getting cached data
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4

## Create a special matrix that is a list of functions to...
## - set the value the matrix
## - get the value the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {

  inverse <- NULL
  set <- function(n) {
    m <<- n
    inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Calculate the inverse of a special matrix.
## If the inverse has already been found, return the cached version.
## If the inverse has not been found, find the inverse, cache the inverse,
## and return the inverse.

cacheSolve <- function(x, ...) {  
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  inverse
}

#
# playing w/matrices
#
# m <- matrix(c(4,3,3,2), nrow = 2, ncol = 2, byrow=TRUE)
#      [,1] [,2]
# [1,]    4    3
# [2,]    3    2
#
# solve(m)
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
