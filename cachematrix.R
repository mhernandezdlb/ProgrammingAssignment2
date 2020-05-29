## This functions cache the inverse of a given matrix
## They are useful for repetitive computations 

## makeCacheMatrix creates a special matrix that is
## a list with a function to:
## set and get the value of the matrix and 
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## cacheSolve calculates de inverse if the previous
## matrix. It first checks if the inverse was 
## already calculated. If so, it gets the inverse 
## from the previous list showing a meesage. If not,
## it calculates the inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
