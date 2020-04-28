## Catching the inverse of a vector
## With following functions, we'll cache the inverse of a matrix rather than computing it 
## again and again which is a costly computation that we can avoid with the help of following functions. 

## This function creates a special matrix object that can cache its reverse. 
## Our matrix contains a function to set the value of the matrix, get the value of the matrix, 
##set the value to inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns the inverse if the matrix returned by the above function (makeCacheMatrix). 
## If the inverse has already beed calculated, the this function should return the value from the cahce
## without any additional computation.

cachesolve <- function(x, ...) {
        inverse <- x$getinverse()
        ## if the inverse has already been calculated, it's going to return the message "getting catch data"
        ##and it won't be computing the inverse again.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}

