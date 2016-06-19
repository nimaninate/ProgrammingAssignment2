## This piece of code contains two functions that are used to
## calculate the inverse of a special matrix object or retrieve 
## the inverse matrix from the cache.

## The first function,  makeCacheMatrix creates a special matrix , 
## which is really a list containing a function to:
## 1.set is a function to store the changes on matrix.
## 2.get is a function to return the matrix in the main function.
## 3.setinverse is a function which calculates or retrive the inverse of the matrix.
## 4.getinverse is a function which returns the inversed matrix.

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


## The following function calculates the mean of the special matrix
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the inversed matrix in the cache via the
## setinverse function.

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
