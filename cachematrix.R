## Put comments here that give an overall description of what your
## functions do

## theCacheMatrix function returns a list of functions allowing the user to set a Matrix and cache its inverse within the makeCacheMatrix environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## the set function takes a new matrix as an argument and resets the cached mean to NULL (in the makeChacheMatrix environment)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## the get function returns the matrix x
    get <- function() x
    ## sets the cached mean object 'm' (in the makeCacheMatrix environment to its argument 
    setinverse <- function(inverse) inv <<- inv
    ## 
    getinverse <- function() inv
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes a cacheMatrix object (as returned by makeCacheMatrix) and if the matrix is square, returns its inverse cacheing the result to reuse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## checks whether the chached inverse is NULL (a NULL value is reset every time a new matrix is stored to avoid returning cached results for the wrong matrix)
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
