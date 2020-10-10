## The following functions - makeCacheMatrix and cacheSolve - are able to set
## the value of the matrix, create an inversion matrix and cache the result so
## that it could be used at any point of time without recomputing the value.

## makeCacheMatrix - This function can set and get the value of the input matrix
## and set and get the value of inversion matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
  getInverseMatrix <- function() inverseMatrix
  
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## cacheSolve - This function checks if the inversion matrix is already created.
## If not, it uses solve() method to invert the matrix. It then sets and returns
## the value of inversion matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)){
      message("Getting cached data")
      return(inverseMatrix)
  }
  matrix <- x$get()
  inverseMatrix <- solve(matrix, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
