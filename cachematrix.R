## makeCacheMatrix - creates a special "matrix" object
## cacheSolve - computes the inverse of the special "matrix"

## Function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Inverse matrix initialization
  invMatrix <- NULL
  ## Set input matrix
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  ## Obtaining input matrix
  get <- function() x
  ## Setting inverse matrix
  setInvMatrix <- function(invMatrixLocal) invMatrix <<- invMatrixLocal
  ## Obtaining inverse matrix
  getInvMatrix <- function() invMatrix
  ## Creating result list
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Obtaining inverse matrix or NULL from object x
  invM <- x$getInvMatrix()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invisible(invM)) ## invisible - to supress output
  }
  ## Calculating inverse matrix if invM is NULL
  data <- x$get()
  invM <- solve(data, ...)
  message("calculating matrix inverse")
  x$setInvMatrix(invM)
  ## Return a matrix that is the inverse of 'x'
  invisible(invM) ## invisible - to supress output
}
