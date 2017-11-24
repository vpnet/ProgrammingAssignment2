## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initialize the cache
  cache <- NULL
  # implement set, get, inverse functions
  set <- function(y) {
    x <<- y
    # flush cache
    cache <<- NULL
  }
  # just return stored value
  get <- function() x
  
  setinv <- function(z) cache <<- z
  
  # get the cache value
  getinv <- function() cache
  
  # return a list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  cache <<- x$getinv()
  
  if(!is.null(cache)){
    return(cache)
  }
  mat <- x$get()
  cache <- solve(mat, ...)
  x$setinv(cache)
  cache
}
