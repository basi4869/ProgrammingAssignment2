## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m.inv <- x$get.inv()
  if (!is.null(m.inv)) {
    message("Retrieving from cache")
    return(m.inv)
  }
  data <- x$get()
  m.inv <- solve(data, ...)
  x$set.inv(m.inv)
  return(m.inv)
}
