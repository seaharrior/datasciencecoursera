## Programming Assigment 2 - Caching the Inverse of a Matrix
## Two special functions are created to enable the calculation of the inverse of a matrix and caching
## the result.


## "makeCacheMatrix" function creates a "special" matrix that includes getter and 
## setter functions that enable caching the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

 
## "cacheSolve" computes the inverse of the "special" matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, this function will retrieve the results,
## otherwise it would calulate the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m) ## Return a matrix that is the inverse of 'x' that has been cached.
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x' that has been calculated.
}
