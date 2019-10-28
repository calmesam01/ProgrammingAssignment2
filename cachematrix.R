##The main aim is to create a pair of functions i.e.
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

##makeCacheMatrix is a function that creates a special object-matrix that caches its 
##inverse for the input (that is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function that computes the inverse of the special matrix 
## returned by the above function-makeCacheMatrix(). If the inverse has already been calculated 
## (and the matrix has not been changed) then, the cacheSolve function should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
