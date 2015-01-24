## This function inputs a matrix and returns a list of functions:
## setmatrix: sets the value of a matrix
## getmatrix: gets the values inside a matrix
## setinv: does matrix inversion
## getinv: gets inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}


## This function returns inverse of matrix created using makecacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
