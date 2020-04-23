## Quickly perform matrix inversion by caching inverted matrices and retriving them for future tasks instead of
## computing the inverse anew each time

## The function makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y){
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve either computes the inverse of the matrix returned by makeCacheMatrix 
## or retrieves the already computed inverse from the cache 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
