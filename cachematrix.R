## Recomputing inverse of same matrix over and over can be time consuming operation
## if the matrix is big.
## These functions will cache the inverse of a matrix to save some time and effort in that case

## makeCacheMatrix(x) creates a a special list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL # initializing inv
      
      set <- function(y){ # set matrix in the cache
            x <<- y 
            inv <<- NULL
      }
      
      get <- function() x
      # return matrix
      
      setinv <- function(inverse) inv <<- inverse
      # take inverse value of a matrix and save it
      
      getinv <- function() inv
      # return inverse value
      
      list(set = set, get=get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve(x,...) will compute returned value from makeCacheMatrix.
## if the inverse already had been calculated, inverse should be retrieved from the cache

cacheSolve <- function(x, ...) {
      
      inv <- x$getinv() # put whatever is in the cache
      
      if(!is.null(inv)){ # check if there's inverse of the matrix in cache
            message("getting cached data")
            return(inv) # if there is, returns inverse.
      }
      
      data <- x$get() #if not, get the original matirx
      
      inv <- solve(data) # compute the inverse
      
      x$setinv(inv) # save inverse in the cache
      
      inv # Return a matrix that is the inverse of 'x'
}






