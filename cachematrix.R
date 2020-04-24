## Source contains two functions:  
## Calculates and returns inverse of a matrix; 
## For repeated calls, results are returned from cache stored prior.

## makeCacheMatrix: Stores the result in cache for repeated calls.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(invmatrix) m <<- invmatrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: Search in cache for inverse of matrix obj, if found return
##    otherwise create the inverse, store in cache, and return inverse to caller

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
