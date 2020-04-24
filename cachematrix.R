##The following two functions can be used to cache the inverse of a matrix
# They are useful to avoid unecessary computation: if the matrix hasn't changed
# the function return the cached value of the inverse

##The first function, makeCacheMatrix creates a list containing a function to
#
# - set the matrix
# - get the matrix
# - set the inverse of the matrix
# - get the inverse of the matrix
# 
# When set a matrix also delete the value of the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function cacheSolve return the inverse of a matrix. 
#  If there is a cached value return that matrix without a new computation
#  Othewise it compute the new inverse that will be store

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}
