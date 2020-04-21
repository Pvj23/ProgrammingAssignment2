## Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly. The following are functions that can help out solving this problem.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
set <- function(y){
    
    x <<- y
    inv <<- NULL
}

get <- function()x

setinverse <- function(inverse) inv <<- inverse
getinverse <- function()inv
list(set = set,
     get = get,
     setinverse = setinverse, 
     getinverse = getinverse)
}



##  This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
        
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    M <- x$get()
    
    inv <- solve(M, ...)
    
    x$setInverse(inv)
    
    return(inv)
}


