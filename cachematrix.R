## makeCacheMatrix function accepts a Matrix as the argument and returns a list of 
## four functions - get, set, getinverse and setinverse. 
## get - to get the supplied matrix
## set - to set the matrix into the Cache
## getinverse - to get the inverse of the supplied matrix
## setinverse - to set the inverse of the supplied matrix into the Cache


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    ## Returns a list of four functions
}


## cacheSolve function returns the inverse of a matrix. If the inverse exists in the cache, 
## it retunrs it form the Cashe, else it gets the matrix by using the getinverse function 
## in the makeCacheMatrix function and returns the inverse using the 'Solve' function in R
## and by setting the value of the inverse matrix through the setinverse function in the 
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    print(data)
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
