## This following function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(t) m<<- t
        getsolve <- function() m
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- t(data, ...)
        x$setsolve(m)
        m
}

