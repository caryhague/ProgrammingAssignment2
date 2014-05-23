## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve uses the cache if available to solve the inverse of the matrix

## function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
    ## set the value of the vector
    ## get the value of the vector
    ## set the value of the mean
    ## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    time1 <- proc.time() 
    m <- x$getinv()
    if(!is.null(m)) {
        ## get the cached data
        cat("Time:", proc.time()-time1)
        return(m)
    }
    mydata1 <- x$get()
    m <- solve(mydata1, ...)
    x$setinv(m)
    cat("Time:", proc.time() - time1)
    return(m)
}
