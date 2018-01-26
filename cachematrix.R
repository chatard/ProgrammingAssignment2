## This function creates a special "matrix" object that can cache its inverse.

## creates an R object that stores a square-matrix.

## usage example : makeCacheMatrix(c(1, 4, 6, 8)) **
## usage: 1) one can use a new matrix mxa <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2).
## 2) mxb <- makeCacheMatrix(mxa) .
## 3) cacheSolve(mxb) to put mxb in cache.
## 4) cacheSolve(mxb) a second time to get mxb frome cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}
## The following function calculates inverse of the special "vector"  created with makeCacheMatrix:
## cacheSolve() : runing this function a first time => put in cache..
## runing this function a second time => get inv. matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("comes from cached data : ")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
