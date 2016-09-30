## Special implementation of matrix object
## which can cache it's inverted counterpart

## Creates cacheable matrix object

makeCacheMatrix <- function(x = matrix()) {
  
  #x = matrix which can be inverted
  
  #this is the cached inverted matrix
  cachedInvMatrix <- NULL
  
  #setter
  #y is the original matrix
  set <- function(y) {
    x <<- y
    cachedInvMatrix <<- NULL
  }
  
  #getter
  get <- function() x
  
  #setter for the inverted matrix
  setInvMatrix <- function(inv_mtrx) cachedInvMatrix <<- inv_mtrx
  
  #getter for the inverted matrix
  getInvMatrix <- function() cachedInvMatrix
  
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## checks if we already have a inverted matrix
## of the x, if not we invert the x and cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x must be returned by makeCacheMatrix
  
  #do we have cached matrix already?
  invMtrx <- x$getInvMatrix()
  if(!is.null(invMtrx)) {
    #we have cached matrix
    #return it
    message("getting cached data")
    return(invMtrx)
  }
  
  #we dont have
  #get the matrix and invert it
  data <- x$get()
  invMtrx <- solve(data)
  
  #cache and return the inverted
  x$setInvMatrix(invMtrx)
  
  return (invMtrx)  
}
