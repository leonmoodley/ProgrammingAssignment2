
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{ 
  temp  <- NULL 
  set  <- function(y)
  { 
    x <<- y 
    temp <<- NULL  
  } 
  get  <- function() x 
  setinverse  <- function(inverse) temp  <<- inverse 
  getinverse  <- function() temp 
  list(set= set, get = get,setinverse = setinverse, getinverse = getinverse) 
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix  above. 
## If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  { 
  temp  <- x$getinverse() 
  if (!is.null(temp))
    { 
      return(temp) 
    } 
  data  <- x$get() 
  temp  <- solve(data, ...) 
  x$setinverse(temp) 
  temp 
}

