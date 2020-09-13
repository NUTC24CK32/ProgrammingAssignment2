## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL                                 ## s is the inverse of matrix; set to NULL
    set <- function(y){
            x <<- y
            s <<- NULL
    }
    get <- function() x                       ## get the matrix object "x"   
    
    setinv <- function(inverse) s <<- inverse ## use the inverse function to set the inverse matrix to s
    
    getinv <- function() s                    ## get the inverse matrix s
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache..

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        s <- x$getinv()              ## get the inverse matrix to s  
        
        if(!is.null(s)){             ## check for the s value
          
              message("getting cached data") ## will be printed if we have the data
          
              return(s)                     ## inverse matrix s will be returned
        }
        data <- x$get()                     ## matrix object will be put into data  
        
        s <- solve(data,...)                ## inverse matrix will be put into s
        
        x$setinv(s)                         
        
        s                                   ## inverse matrix will be returned
}
