 ## Because inverse of a mtrix is a resource intensive operation,
 ## there is a benefit of caching the invers of the matrix instead of
 ## computing it repeatedly. The functions below will check whether inverse
 ## is already available in cache and if so, results will be returned with
 ## message "getting cached data." Otherwise, inverse will be computed.
 
 ## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
 
 makeCacheMatrix <- function(x = matrix()) { 
      inv <- NULL 
      set <- function(y) { 
          x <<- y 
          inv <<- NULL 
      } 
      get <- function() x 
      setinverse <- function(inverse) inv <<- inverse 
      getinverse <- function() inv 
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
 } 
 
 
 ## cacheSolve function computes the inverse of the special "matrix" returned 
 ## by makeCacheMatrix above. If the inverse has already 
 ## been calculated (and the matrix has not changed), 
 ## then the cachesolve should retrieve the inverse from the cache.
 
 cacheSolve <- function(x, ...) { 
      inv <- x$getinverse() 
      if(!is.null(inv)) { 
          message("getting cached data.") 
          return(inv) 
      } 
      data <- x$get() 
      inv <- solve(data) 
      x$setinverse(inv) 
      inv 
  } 
 
 
 
