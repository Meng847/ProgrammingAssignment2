## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y){
                x <<- y
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix, 
             setInverse = setInverse, 
             getInverse = getInverse
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        return(m)
}
