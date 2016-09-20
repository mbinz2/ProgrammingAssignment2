## This series of functions creates a way to avoid needing to recalculate
## the inverse matrix once it has already been cached and stored in the object

## The first function takes a matrix object as an argument and returns a list
## that includes the information on the matrix and it's inverse once calculated

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The second function attempts to solve (calculate the inverse of the matrix - assuming
## the matrix is always invertable in this case) the matrix if the solution is not already
## in the cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
