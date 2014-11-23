## Caching inverse of a matrix

## Functions that cache inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<- inverse
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## Retrieve from cache

cacheSolve <- function(x, ...) {
  ## Computes the inverse of the matrix returned by makeCacheMatrix(), 
  ## unless the inverse has already been calculated, in which case retrieves from cache
  
  
  m <- x$getinverse()
  
  if ( ! is.null(m)) {
    
    print("getting cached data")
    
    return(m)
    
  }
  
  m <- solve(x$get())
  
  x$setinverse(m)
  
  m
}
