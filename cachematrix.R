## Put comments here that give an overall description of what your
## functions do

# 'Set' method will compare existing cachecd data with new input to recreate cache.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      if(!identical(y,x)){
            x <<- y
            m <<- NULL
      }
  }
  get <- function() x
  setmatrix <- function(d) m <<- d
  getmatrix <- function() m
  list(set=set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Write a short comment describing this function
# No change to this method except cacluating inverse with solve method.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getmatrix()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
