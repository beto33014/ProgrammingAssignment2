## A pair of functions that cache the inverse of a matrix

## Function that creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

##  Function that computes the inverse of the special matrix returned by 
##  the function makeCacheMatrix.  If the inverse has already been calculated
##  cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

