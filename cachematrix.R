
## Creates a special "matrix" object that can cache its inverse.
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize to NULL
  cache <- NULL
  
  ## Define function to set the value of the matrix. It also clears the old
  ## inverse from the cache
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Invert the matrix and store in cache 
  set_Matrix <- function(solve) cache <<- solve
  get_Inverse <- function() cache
  
  ## Return a list to the working environment 
  list(set = set, get = get,
       set_Matrix = set_Matrix,
       get_Inverse = get_Inverse)
}

## Return a matrix that is the inverse of 'x'
## Computes the inverse of  matrix if it does not exist in cache. 
## And then stores it in the cache 
cacheSolve <- function(x, ...) {
  
  ## Attempt to get the inverse of the matrix stored in cache
  cache <- x$get_Inverse()
  
  ## If the cache was not empty, we can just return it
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  
  ## Else calculate it, cache it, and then return it.
  data <- x$get()
  cache <- solve(data, ...)
  x$set_Matrix(cache)
  
  return(cache) 
}