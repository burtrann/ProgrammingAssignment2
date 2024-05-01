## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1. The set function sets the matrix value.
# 2. The get function retrieves the matrix.
# 3. The setinverse function computes and caches the inverse.
# 4. The getinverse function retrieves the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function() m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}



## Write a short comment describing this function
# If you create an object using the makeCacheMatrix function, 
# you can then utilize the cacheSolve function to calculate the inverse of that matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    ## Return a matrix which is the inverse of 'x'
    x$setinverse(m)
    
    m
}
## Return a matrix that is the inverse of 'x'
