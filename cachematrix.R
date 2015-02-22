## The makeCacheMatrix function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversematrix<-NULL  #inverse matrix set to null
    set<-function(y) {
        x<<- y
        i.m<<- NULL
    }
    get <- function() x
    setinversematrix <- function(solve) i.m <<- solve
    getinversematrix <- function() inversematrix
    list(set=set, get=get, setinversematrix = setinversematrix, 
         getinversematrix = getinversematrix)
}


## The cacheSolve function computes the inverse of 
## the matrix returned by makeCacheMatrix; if the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
      
        inversematrix <- x$getinversematrix()
        if(!is.null(inversematrix)) {
            message("getting cached data")
            return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinversematrix(inversematrix)
        inversematrix
    
}
