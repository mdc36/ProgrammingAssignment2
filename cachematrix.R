## Computes the inverse of a square matrix 'x' and caches it to 'm'
## Checks if the inverse of 'x' has already been cached
## If not, computes the inverse.  
## Otherwise retrieves the inverse from the cache


## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## creates an object 'm' to which an inversed matrix can be cached
    m <- NULL
    
    ## Replaces the matrix and sets m <<- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ## Gets the matrix 'x'
    get <- function() x 
    
    ## Sets the inverse of 'x' to 'm'
    setinverse <- function(solve) m <<- solve
    
    ## Gets the inverse of 'x' from the cache
    getinverse <- function() m
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Checks if the inverse of 'x' was already cached
## If not, computes the inverse 
## Prints the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    ## Gets the inverse of 'x'
    m <- x$getinverse()
    ## Checks if the inverse of matrix x was cached in makeCacheMatrix
    if(!is.null(m){
        message("getting cached data")
        
        ## Returns the cached matrix
        return(m)
    }
    
    ## If the inverse was cached in makeCacheMatrix
    ## Gets 'x'
    data <- x$get()
    ## Sets the inverse of 'x' to 'm'
    m <- solve(data, ...)
    x$setinverse(m)
    ## Prints the inverse matrix
    m
}