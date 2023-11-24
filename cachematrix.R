## makeCacheMatrix creates a cache of the inverse of a matrix to be use 
## as computing the inverse of a matrix takes a lot of CPU

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
## If the inverse has already been calculated
## and the matrix has not changed it returns the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting and returning cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
