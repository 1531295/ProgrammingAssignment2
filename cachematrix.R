## The following functions will take advantage of R scoping rules.
## 'makeCacheMatrix' and 'cacheSolve' are functions that can be used
## to cache the inverse of a matrix.
## 

## The function makeCacheMatrix creates a special 'matrix' object
## that can cache its inverse.

## The matrix can be accessed and changed via get() and set()
## The inverse of the matrix can be accessed and changed via
## getInv() and setInv()

makeCacheMatrix <- function(x = matrix()) {
  #cached inverse
  inv <- NULL
  
  set <- function(newX){
    x <<- newX
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  
  
  #return list
  list(get=get, set = set, getInv=getInv,setInv = setInv)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv
  
  if(!is.null(inv)){
      message('getting cached data')
      return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
