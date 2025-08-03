## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Cached inverse matrix
  
  # Set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear cached inverse
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Return a list of the above functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse...")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)  # Cache the inverse
  inv
}

