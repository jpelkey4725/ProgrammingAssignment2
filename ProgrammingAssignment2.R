
  ## This function will cache a given matrix that is passed to the function
  
  makeCacheMatrix <- function(x = matrix()) {
    
    #Set inverse matrix 'inv' to Null.  
    
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      
      #using solve function to get inverse
      
      get <- function() x
      
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
  
    }
  
  
cacheInv <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
  ## return inverse of matrix 'inv'
}

