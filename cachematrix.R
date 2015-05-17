## The first function, makeCacheMatrix creates a special "matrix" object, 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## setInvMatrix the value of the Inverse of Matrix
## getInvMatrix the value of the Inverse of Matrix

## The second function, cacheSolve computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## Note - Matrix given as input to makecacheMatrix and set should be 
## a Square Matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      InvMatrix <- NULL
      set <- function(SqMatrix) {
            x <<- SqMatrix
            InvMatrix <<- NULL
      }
      get <- function() x
      
      setInvMatrix <- function(InverseMatrix) InvMatrix <<- InverseMatrix
      
      getInvMatrix <- function() InvMatrix
      
      list(set = set, get = get,
           setInvMatrix = setInvMatrix,
           getInvMatrix = getInvMatrix)
}


## Return a matrix that is the inverse of 'x'. If already computed then
## retrieves it from cache, otherwise calculates it.

cacheSolve <- function(x, ...) {
      InvMatrix <- x$getInvMatrix()
      
      if(!is.null(InvMatrix)){
            message("getting cached data...")
            return(InvMatrix)
      }
      
      SqMatrix <- x$get()
      InvMatrix <- solve(SqMatrix, ...)
      x$setInvMatrix(InvMatrix)
      InvMatrix
}
