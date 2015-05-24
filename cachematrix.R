## Put comments here that give an overall description of what your
## functions do

## Creates a list-matrix object that can cache its inverse
## Takes a matrix object as its input; must be square matrix

makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m.inv <<- inverse
  getinverse <- function() m.inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the list-matrix object returned by makeCacheMatrix
## If inverse has already been calculated and the matrix hasn't changed, retrieves 
## the cached inverse of the matrix. 
## Function input is the returned value of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m.inv <- x$getinverse()
  if(!is.null(m.inv)) {
    message("getting cached data")
    return(m.inv)
  }
  data <- x$get()
  m.inv <- solve(data, ...)
  x$setinverse(m.inv)
  m.inv
}
