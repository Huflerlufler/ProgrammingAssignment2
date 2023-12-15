# This is an my solution on assignment 2, created 15-12-2023

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a matrix object that can be cashed
  
  inv <- NULL
  
  # Set the elements of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the elements of the matrix
  get <- function() {
    x
  }
  
  # Set the elements of the matrix inverse
  set_inverse <- function(inverse) {
    inv <<- inverse
  }
    
  # Get the elements of the matrix inverse
  get_inverse <- function() {
    inv
  }
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...){
  # This functions returned a matrix that is inverted
  inv <- x$get_inverse()
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$set_inverse(inv)
  inv
}
