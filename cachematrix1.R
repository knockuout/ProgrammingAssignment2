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


## 'cacheSolve()' returns the inverse of a given matrix, x. It checks first to see if the
## inverse of the matrix was already created. If so, it will get the cached results and skips 
## computing the inverse.  Otherwise, it calculates the inverse of the matrix using solve()
## and sets the value in the cache via setinverse() function.  This function assumes that
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
## Sample usage:
## > mymatrix <- rbind(c(1, 2), c(-2, -1))      #created a matrix
## > mymatrix                                   #viewed the matrix
## [,1] [,2]
## [1,]    1    2
## [2,]   -2   -1
## > mcm <- makeCacheMatrix(mymatrix)           #assigned makeCacheMartix function to mcm
## > mcm$get()                                  #checked get feature to view matrix
## [,1] [,2]
## [1,]    1    2
## [2,]   -2   -1
## > cacheSolve(mcm)                            #ran cacheSolve to return the inverse (nothing in cache)                                 
## [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,]  0.6666667  0.3333333
## > cacheSolve(mcm)
## getting cached data                          #ran cacheSolve to return the inverse (using cached data)
## [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,]  0.6666667  0.3333333
## > 


