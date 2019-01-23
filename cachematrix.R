## cacheMatrix creates a function makeCacheMatrix that initializes 4 functions:
##  get, setinverse, getinverse and list.
## Then, creates another function cacheSolve that gets the cached data if the
##  program has already been executed.

## Create a NULL variable to hold cache.  

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }

##  Create 4 functions - get, setinverse, getinverse, list.
    get <- function() x
    setinverse <- function(inverse) c <<- inverse
    getinverse <- function() c
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
    c <- x$getinverse()
    if (!is.null(c)) {
        message("getting cached data")
        return(c)
    }
    data <- x$get()
    c <- solve(data, ...)
    x$setinverse(c)
    c

}

##  Test functions.
##  Generate random numbers.
set.seed(250)
r <- rnorm(36)
## Create matrix and assign to variable "b".
b <- matrix(r, nrow=6, ncol=6)
## Run function makeCacheMatrix and create variable "a".
a <- makeCacheMatrix(b)

##  Run cacheSolve twice to get cache and get cached data.
cacheSolve(a)
cacheSolve(a)
