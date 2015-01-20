## Matrix inversion is usually a costly computation & there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly( via looping,for example).  These two functions are used to
## cache the inverse of a matrix
##' 
##' 'makeCacheMatrix()' creates a list containing a function to:
##' 1.  set the value of the matrix
##' 2.  get the value of the matrix
##' 3.  set the value of inverse of the matrix
##' 4.  get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invm <<- inverse
    getinverse <- function() invm
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 'cacheSolve()' returns the inverse of a given matrix x. It checks first to see if the
## inverse of the matrix is already created. If so, it will get the cached results and skips 
## computing the inverse.  Otherwise, it calculates the inverse of the matrix
## and sets the value in the cache via the `setinverse` function.  This function assuemes
## the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinverse()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinverse(invm)
    invm
}
