## The below written functions cache and compute the inverse of the matrix
## provided as an input

## This function(makeCacheMatrix) creates a "matrix" object that 
## is able to cache its inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtrx <<- x
    inverse <<- NULL
  }
  get <- function() return(mtrx)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## The function written below(cacheSolve) finds the inverse of the "matrix"
## returned by  `makeCacheMatrix` function written above. If the inverse has
## already been calculated and the matrix has not changed, then `cacheSolve` 
## should retrieve the inverse from the cache.


cacheSolve <- function(mtrx, ...) {
  inverse <- mtrx$getinv()
  if(!is.null(inverse)) {
    message("Now fetching cached data...")
    return(inverse)
  }
  data <- mtrx$get()
  inverse <- solve(data, ...)
  mtrx$setinv(inverse)
  return(inverse)
}