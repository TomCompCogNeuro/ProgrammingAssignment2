## The goal of these fucntions is reduce computing time 
## by limiting potentially time-consuming computations,
## in this case matrix inversion


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      InvM <- NULL
      set <- function(y) {
            x <<- y
            InvM <<- NULL
      }
      get <- function() x
      setINV <- function(solve) InvM <<- solve
      getINV <- function() InvM
      list(set = set, get = get,
           setINV = setINV,
           getINV = getINV)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
      InvM <- x$getINV()
      if(!is.null(InvM)) {
            message("getting cached data")
            return(InvM)
      }
      data <- x$get()
      InvM <- solve(data, ...)
      x$setINV(InvM)
      InvM
}
