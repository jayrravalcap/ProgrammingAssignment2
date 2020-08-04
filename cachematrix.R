## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverseMatrix as NULL value
  inverseMatrix <- NULL
  # get function to get Matrix
  get <- function() {
    x
  }
  
  # set function to set a new Matrix
  set <- function(newMatrix) {
    x <<- newMatrix
    inverseMatrix <<- NULL
  }
  
  # getInverse to get InverseMatrix
  getInverse <- function() {
    inverseMatrix
  }
  
  # setInverse with Inverse Matrix solution
  setInverse <- function(solvedMatrix) {
    inverseMatrix <<- solvedMatrix
  }
  
  #return a list of functions
  invisible(list(get = get, set = set, 
                 getInverse = getInverse, 
                 setInverse = setInverse))
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix() 
## above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from 
## the cache

cacheSolve <- function(x, ...) {
  # querying and asssigning inverseMatrix cached value
  inverseMatrix <- x$getInverse()
  
 # checking value of cache and using it if not NULL
 if(!is.null(inverseMatrix)) {
    message("getting cached Inverse Matrix data ....")
    return(inverseMatrix)
 }
 
 # gettting matrix and calculating inverse
 data <- x$get()
 inverseMatrix <- solve(data, ...)
 # set value on inverse matrix in cache
 x$setInverse(inverseMatrix)
  
 ## Return a matrix that is the inverse of 'x'
 return(inverseMatrix)
}