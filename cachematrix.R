## The functions below create a special object to cache the inverse of a matrix.
## Like any other cache, if the inverse has already been calculated, its value is retrieved.
## Otherwise, the inverse is calculated and stored in the cache.

## This function creates the matrix cache
makeCacheMatrix <- function(x = matrix()) {

  ## Tests if the supplied argument is a matrix
	if(is.matrix(x))
	{
    inverse <- NULL
    mat <- x
  } else
  {
    print("Supplied argument not a matrix. Using empty matrix instead")
    inverse <- NULL
    mat <- matrix()
  }
  
  set <- function(x = matrix()) {
	if(is.matrix(x))
	{
    print("Supplied argument not a matrix. Using empty matrix instead")
	  inverse <<- NULL
    mat <<- x
  } else
  {
    inverse <<- NULL
    mat <<- matrix()}
  }
  get <- function() mat
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function does the calculation of the inverse and stores its value, or rretrieve
## the inverse, if it has already been calculated.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  orig_matrix <- x$get()
  inv <- solve(orig_matrix, ...)
  x$setinverse(inv)
  inv
}