## The purpose of this script is to create a procedure for saving time 
## to calculate the inverse of a matrix. By combining the two functions "makeCacheMatrix" and "cacheSolve",
## the inverse of an arbitrary matrix will be calculated only once and cached in the memory.
## The next time the matrix inverse is needed, it will be retrieved from the memory, rather than being calculated again.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The object enlists 4 functions:
## (1)set the value of the invertible matrix
## (2)get the value of the invertible matrix
## (3)set the inverse of the invertible matrix
## (4)set the value of the invertible matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
      set <- function(y) {
              x <<- y
              inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
      if(!is.null(inverse)) {
             message("getting cached data")
             return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
