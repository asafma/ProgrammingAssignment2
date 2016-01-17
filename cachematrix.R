## This code caches the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## This function computes the inverse of the matrix returned by makeCacheMatrix
## above, retrieving the inverse from the cache if it has already been calculated.  

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  }
  data <- x$getinverse()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}