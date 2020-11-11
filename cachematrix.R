## Caching the Inverse of a Matrix:
## Below are pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.

## Created a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function (y) {
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## The function below computes the inverse of the special MATRIX created by
## the function above, "makeCacheMatrix".  It calculates the mean of the 
## special matrix created by the above function.  First, it checks if the mean
## has already been calculated, then it gets the mean from the cache and skips
## the computation.  Otherwise, it calculates the mean of the data and sets the
## value of the mean in the cache via the setmean function.  
## *(If the inverse has already been computed (matrix still the same), 
## it would retrieve the inverse from the cache data.)*

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setInverse(inv)
        inv
}
