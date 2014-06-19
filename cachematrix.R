
## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #set inv to null
  
  ##function that sets the value of the matrix to the value of y
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##funtion that returns the value of the matrix
  get <- function() x
  
  #function that sets the value of the cached matrix inverse to the value of inverse
  setInverse <- function(solve) inv <<- solve
  
  #function that returns the value of the cached matrix inverse
  getInverse <- function() inv
  
  #compose the list of function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  #check if a cached matrix exists...
  if(!is.null(inv)) {
    #...and return it if found
    message("cached data found, return it!")
    #exit function
    return(inv)
  }
  
  #...else do the calculation
  data <- x$get()
  inv <- solve(data, ...)
  
  #save the result in the cache...
  x$setInverse(inv)
  
  #...and return the result
  inv
}
