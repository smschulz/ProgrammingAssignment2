## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The function makeCacheMatrix creates a special matrix object
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix using solve
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   		
  set <- function(y) {	
    x <<- y		
    m <<- NULL		
  }				
  get <- function() x	
  setinverse <-function(solve) m<<- solve	
  getinverse <- function() m		
  list(set = set, get = get,		
       setinverse = setinverse,		
       getinverse = getinverse)	
}


## The function cacheSolve computes the inverse of the special matrix returned
## by makeCacheMatrix. However, it first checks to see if the inverse has 
## already been calculated. If so, it uses the get command to get the inverse
## from the cache and skips the computation. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()  		
  if(!is.null(m)) {				
    message("getting cached data")	
    return(m)				
  }						
  data <- x$get()				
  m <- solve(data, ...)			
  x$setinverse(m)				
  m	
}
