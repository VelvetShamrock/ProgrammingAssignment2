## Matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return matrix inverse 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Retrieving data from the cache instead")
    return(i)
  }
  myMatrix <- x$get()
  i <- solve(myMatrix, ...)
  x$setInverse(i)
  i
}