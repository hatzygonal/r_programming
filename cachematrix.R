## Caching the inverse of a Matrix
## Marjan Biocanin 
# R-3.1.1. win7

## makeCacheMatrix function creates special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setsolv <- function(solv) a <<- solve
  getsolv <- function() a
  list(set = set, 
       get = get,
       setsolv = setsolv,
       getsolv = getsolv)
  
  
}


## cacheSolve function computes the inverse of previously created cache matrix
## Return a matrix that is the inverse of 'x'
## if the inverse has already been calculated, cacheSolve should
## retreive the inverse from the cache
## otherwise returns the inverse of the matrix

cacheSolve <- function(x=matrix(), ...) {
  
  
  a <- x$getsolv()
  if(!is.null(a)) {
      message("getting cached data")
      return(a)
  
  }
  solv <- x$get()
  a <- solve(solv, ...)
  x$setsolv(a)
  a
  
}
