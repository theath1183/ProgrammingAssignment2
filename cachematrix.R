## Two functions: makeCacheMatrix and cacheSolve. 

##The makeCacheMatrix function initializes a matrix and its inverse and allows
## other functions (specifically the cacheSolve function) to set/get values for
## the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invx <- matrix()
    set <- function(y){
        x <<- y
        invx <<- matrix()
    }
    get <- function() x
    setinv <- function(inv) invx <<- inv
    getinv <- function() invx
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function will either calculate and cache the inverse of a
## given square matrix or it will pull the inverse from cache.

cacheSolve <- function(x, ...) {
    invx <<- x$getinv()
    if(!(is.na(invx)[1])){
        message("getting cached data")
        return(invx)
    }
    X <- x$get()
    invx <- solve(X)
    x$setinv(invx)
    invx
}
