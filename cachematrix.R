##This program is to create a matrix and cache its inverse to reduce the the computational
##cost of calculating a matrix's inverse.

## makeCacheMatrix creates a matrix and cache its inverse, it contains four functions to
## 1.set the value of the matrix 2.get the value of the matrix
## 3.set the value of the inverse 4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y)
    {
      x<<- y
      inv <<- NULL
    }
    get <- function () x
    setInverse <- function (inverse)
    {
      inv  <<- inverse
    }
    getInverse <- function() inv
    
    list (set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## compute the inverse and cache it if the inverse does not exist
## otherwise, return the cached inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    v <- x$getInverse()
    if (!is.null(v)){
        message ("getting cached data")
        return (v)
    }
    data <- x$get()
    v <- solve(data)
    x$setInverse(v)
    v        
}
