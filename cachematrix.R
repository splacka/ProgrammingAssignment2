## The following functions create a special matirx object and
## compute and cache its inverse as needed to save computation time

## The makeCacheMatrix function creates a special matrix which
## is a list containing a function to 1. set the value of the matrix
## 2. get the value of the matrix 3.set the value of the inverse
## 4. get the value of the inverse

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


## The cacheSolve function calculates the inverseof the special
## matrix created with makeCacheMatrix. It first checks
## to see if the inverse has already been calculated. If so, it 
## gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the
## inverse in the cache via the setmean function.

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
  ## Returns a matrix that is the inverse of 'x'
}
