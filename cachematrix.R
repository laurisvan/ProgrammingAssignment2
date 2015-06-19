## These two functions are utilies for caching a matrix inverse. They can be
## used in place of orinary matrices in case where repetitive inverse matrix
## calculations are needed.
##
## Sample usage:
## m1 = matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
## m2 <- makeCacheMatrix(m1)
## m3 <- cacheSolve(m2)
##
## Adapted from vector sample at https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes and caches the inverse of a "matrix" `makeCacheMatrix`.
## In case the inverse is already calculated, the cached result is returned.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
