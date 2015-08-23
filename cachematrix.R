## The function 'makeCacheMatrix' creates a special object to store a matrix and its inverse in cache, so it can be re-used without calculating it from scratch if the matrix itself has not changed
## The second function 'cacheSolve' verifies if the inverse is in cache and uses it to save time; it caculates the inverse otherwise



## This function creates a special "matrix" that is actually a list with functions to set/get the matrix
## and set/get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse of x is in in the cache /
## if it is, and x has not changed, it reads the inverse from the cache an return that value
## Otherwise i calculates the inverse using the function 'solve'
cacheSolve <- function(x, ...){ 
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
