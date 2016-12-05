## Put comments here that give an overall description of what your
## functions do
## This function is intended to calculate the inverse of the matrix and then cache it
## makeCacheMatrix() will obtain the matrix for the inverse
## cacheSolve() returns the inverse of the matrix

## Write a short comment describing this function
## makeCacheMatrix() obtains the matrix
## functions get(), set(), setinverse() and getinverse are defined
## and then placed in a list
## Hence, makeCacheMatrix() returns the list of functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
## cacheSolve() obtains the list of functions returned by makeCacheMatrix()
## It checks first if there is a calculated inverse matrix
## If it exists, it returns the cached inverse
## If it does not exist, cacheSolve calculates the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
