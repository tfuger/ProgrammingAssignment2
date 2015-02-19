## Programming Assignment 2 - R Programming
## Testing the funcionality of caching variables
## within the scope of a function. (Behaves much like
## a Class in C++)

## function to store a matrix with ability to cache
## its inverse to save processing power.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
  
    setInv <- function(inverse) inv <<- inverse
    
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function to check if the inverse of a stored matrix
## has been cached. If not, utilize solve() to get the
## inverse.

cacheSolve <- function(x, ...) {
    
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
  }
  else {
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setInv(inv)
  }

  return(inv)
  
}
