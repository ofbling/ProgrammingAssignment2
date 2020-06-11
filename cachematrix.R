## These two functions work in harmony where the first one sets and gets the data.
## And the second one checks the disc or stored data and gets the inverse.

## This function creates a list of functions that help in setting and getting
## the main matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function()	 i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function checks that if the inverse is already calculated.
## If yes, then it gets the inverse from the disk, otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting inverse of matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}



