## This function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## A new matrix will start with NULL inverse
    inv <- NULL
    ## To define the matrix on the first call
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## To read the contents of the matrix
    get <- function() x
    ## To set the inverse
    setinverse <- function(inverse) inv <<- inverse
    ## To get the inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    ## Check is inverse is already in the cache
    if(!is.null(inv)) {
        message("getting inversed matrix")
        return(inv)
    }
    ## Inverse is not cached
    data <- x$get()
    ## Computes the inverse of the matrix. 
    inv <- solve(a = data, ...)
    x$setinverse(inv)
    inv
}
