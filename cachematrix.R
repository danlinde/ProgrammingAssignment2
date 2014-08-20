## makeCacheMatrix creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  ## set inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## get inverse of the matrix
  m <- x$getinverse()
  ## check if inverse is already cached and use that data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if no data cached, solve for inverse
  matrix <- x$get()
  m <- solve(matrix, ...)
  ## set inverse of matrix
  x$setinverse(m)
  m
}
