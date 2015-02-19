## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. These two functions provide inverse of matrix caching functionality.

## makeCacheMatrix provides functions to generate inverse of a matrix 
## passed in as a paramete. It also caches the results in the
## parent environment of the current function environment.
## It returns all utility functions like set,get,setInverse and getInverse
## as list parameter
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setInverse = setinverse,
         getInverse = getinverse)
}

## cacheSolve function returns the inverse of the matrix. It first checks
## if matrix inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the matrix and sets the value in the cache
## via the setInverse function.
cacheSolve <- function(x, ...) {
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

##  Sample test results
##  Create a matrix and pass it makeCacheMatrix and store in variable a
##  > c=rbind(c(1, -1/2), c(-1/2, 1))  
##  > a <- makeCacheMatrix(c)
##  
##  Pass variable a to the cacheSolve function to inverse the matrix
##  No cache is used while Running cacheSolve for the first time 
##  > cacheSolve(a)
##  [,1]      [,2]
##  [1,] 1.3333333 0.6666667
##  [2,] 0.6666667 1.3333333
##
##  Running cacheSolve second time results are retrieved from the cache.
##  > cacheSolve(a)
##  getting cached data
##  [,1]      [,2]
##  [1,] 1.3333333 0.6666667
##  [2,] 0.6666667 1.3333333
##
