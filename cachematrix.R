## This function stores an inverse matrix in the cache to save
## computation time down the line.

## The first function creates the matrix, a function within a list that sets/
## gets elements of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function takes the inverse of the newly created matrix above.
## It checks that the inverse has been calculated, then skips the calculation
## if it already has been. If not, it calculates the inverse and puts it in
## the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
