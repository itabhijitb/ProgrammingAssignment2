## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse
##     get the value of the inverse

## Function to creates a cachable matrix
makeCacheMatrix  <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Solves the cachable matrix
## Usage
##      cacheSolve(makeCacheMatrix(a),b,...)

##      ## Default S3 method:
##      > B
##           [,1] [,2]
##      [1,]    1    3
##      [2,]    2    4
##      > cacheSolve(makeCacheMatrix(B))
##           [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > 
cacheSolve  <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, , ...)
        x$setinv(m)
        m
}
