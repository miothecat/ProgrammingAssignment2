## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, makeCacheMatrix, creates a list containing a function to
## 1. set the value of the square matrix
## 2. get the value of the square matrix
## 3. set the value of the its inverse
## 4. get the value of the its inverse

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


## Write a short comment describing this function

## The function, cacheSolve calculates the mean of the square matrix
## created with the function makeCacheMatrix. However, it first checks
## to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the square matrix and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
