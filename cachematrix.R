## The following functions provide caching the inverse of a matrix rather than
## computing it repeatedly

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(data) {
        x <<- data
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by `makeCacheMatrix`
## above, it retrieves the inverse if it was already calculated or if the
## hasn't changed
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message('getting cached data')
        return(i)
    }
    data <- x$get()
    i <- solve(data) %*% data
    x$setinverse(i)
    i
}
