## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix() function - Creates a list containing a function to:
#set value of a matrix
#get value of a matrix
#set value of an inverse
#get value of an inverse
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


## Write a short comment describing this function
##cachemean() function:
#calculates the mean of matrix created by makeCacheMatrix()
#first checks to see if inverse is already calculated
#if so, gets the inverse from the cache and skips computation
#otherwise, calculates inverse of data and sets the value of the inverse in the cache via setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
