## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## m <- NULL
    s <- NULL
    set <- function(y){
            x <<- y
            
            ## m <<- NULL
            s <<- NULL
    }
    get <- function() x
    
    ## setmean <- function(mean) m <<- mean
    setinv <- function(solve) s <<- solve
    
    ## getmean <- function() m
    getinv <- function() s
    
    ## list(set = set, get = get, setmean = setmean, getmean = getmean)
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        ## m <- x$getmean()
        s <- x$getmean()
        
        ## if(!is.null(m)){
        if(!is.null(s)){
          
              message("getting cached data")
          
              ## return(m)
              return(s)
        }
        data <- x$get()
        
        ## m <- mean(data,...)
        s <- mean(data,...)
        
        ## x$setmean(m)
        x$setmean(s)
        
        ## m
        s
}
