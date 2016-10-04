## Put comments here that give an overall description of what your
## functions do

# Following functions will create a way to take matrix as an input and create set of functions
# that will calculate inverse of input matrix.
# In order to make the calculation efficient, inverse of matrix will be cached, and if it is called
# later, the result will be pulled from cache instead of calculating it again.

## Write a short comment describing this function
 
# makeCacheMatrix function creates set of functions that will get/set matrix and also get/set inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Write a short comment describing this function

# 'cacheSolve function will check if inverse of matrix is stored in cached first, and if so, then it will return
# the result from cache.  If not, it will calculate the inverse of matrix and return
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}


