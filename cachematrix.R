## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly.  
## The following pair of functions is to cache the inverse of 
## a matrix.

##  makeCacheMatrix : This function creates 
##  a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL 
    
  set  <- function(y){ 
      x <<- y 
      i <<- NULL  
  } 
  
  get  <- function() x 
  
  setinverse  <- function(inverse) i  <<- inverse 
  
  getinverse  <- function() i 
  
  list(set= set, get = get,  
       setinverse = setinverse,  
       getinverse = getinverse) 
}


##cacheSolve : This function computes the inverse 
## of the special "matrix" returned by  makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix 
## has not changed), then  cacheSolve  should retrieve the 
## inverse from the cache
##
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {  
          
  i  <- x$getinverse() 
  
  if (!is.null(i)){ 
      
      message("getting cached data") 
      
      return(i) 
      
    }
   
  data  <- x$get() 
  
  i  <- solve(data, ...) 
  
  x$setinverse(i) 
  
  i
    
}

## Testing results from Rstudio:

## > mat <- matrix(data = c(8,1,5,6), nrow = 2, ncol = 2)
## > mat2 <- makeCacheMatrix(mat)
## > cacheSolve(mat2)
## [,1]       [,2]
## [1,]  0.13953488 -0.1162791
## [2,] -0.02325581  0.1860465
## > cacheSolve(mat2)
## getting cached data
## [,1]       [,2]
## [1,]  0.13953488 -0.1162791
## [2,] -0.02325581  0.1860465
## > cacheSolve(mat2)
## getting cached data
## [,1]       [,2]
## [1,]  0.13953488 -0.1162791
## [2,] -0.02325581  0.1860465
## > 