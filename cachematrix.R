## These functions calculate the inverse of a matrix and cache the value.
## When called if the inverse is know, the cached value is returned.  Else the 
## value is calculated.

## Makes a vector of functions to get and set the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {        
    m <- NULL         
    set <- function(y) {                 
      x <<- y                
      m <<- NULL         
    }         
    get <- function() x         
    setinverse <- function(inverse) m <<- inverse         
    getinverse <- function() m         
    list(set = set, get = get,             
         setinverse = setinverse,              
         getinverse = getinverse)  
}


## Function returns the inverse of X

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
