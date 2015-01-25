## R Programming Assignment #2



## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## define and sets inverse_matrix
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  ## gets values of matrix
  get <- function() x
  
  ## solves for the inverse of the matrix 
  set_inverse_matrix <- function(solve) inverse_matrix <<- solve
  get_inverse_matrix <- function() inverse_matrix
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## defines inverse_matrix with the inverse of matrix
  inverse_matrix <- x$get_inverse_matrix()
  
  ## error checker to see if inverse_matrix has a value
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  
  ## if no value, solve for inverse of matrix and return
  data <- x$get()
  inverse_matrix <- solve(data)
  x$set_inverse_matrix(inverse_matrix)
  
}