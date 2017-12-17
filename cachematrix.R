## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ai <- NULL
      set <- function(y){
          x <<- y
          ai <<- NULL
      }
      get <- function() x
      setinv <- function(solve) ai <<- solve
      getinv <- function() ai
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

  

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),  
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ai <- x$getinv()
      if(!is.null(ai)) {
            message("getting cached data")
            return(ai)
      }
      data <- x$get()
      ai <- solve(data,...)
      x$setinv(ai)
      ai
}
