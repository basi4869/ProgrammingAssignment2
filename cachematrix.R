## make use of cache when computing the inverse of a matrix

## makeCacheMatrix creates a matrix object whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL
  get <- function() {
    return (x)
  }
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  get.inv <- function() {
    return (m.inv)
  }
  set.inv <- function(inverse) {
    m.inv <<- inverse
  }
  return (list(get = get, 
              set = set, 
              get.inv = get.inv, 
              set.inv = set.inv)
          )
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## the result is feteched from cache if already availabe
## the result is computed and stored in cache otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m.inv <- x$get.inv()
  if (!is.null(m.inv)) {
  # retrieve from cache if m.inv is non-empty
    message("Retrieving from cache")
    return(m.inv)
  }
  # solve for the inverse matrix if the result is not available in cache
  data <- x$get()
  m.inv <- solve(data, ...)
  x$set.inv(m.inv)
  return(m.inv)
}
