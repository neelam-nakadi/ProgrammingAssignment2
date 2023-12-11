## Write a pair of functions that cache the inverse of a matrix.
## We introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.


## Write a short comment describing this function
## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
  #return(m)
}
