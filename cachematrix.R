## The functions here (listed below) are to calculate and chche the inverse of 
## a given matrix. 
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## Note: the matrix supplied is always invertible as an assumption.


## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        cm <- NULL #cm: cache matrix
        set <- function(y) {
                x <<- y
                cm <<- NULL
        }
        get <- function() x
        setcm <- function(cachematrix) cm <<- cachematrix
        getcm <- function() cm
        list(set = set, get = get, setcm = setcm, getcm = getcm)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
## Return: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## check if the cache matrix exists
        cm <- x$getcm()
        if(!is.null(cm)) {
                message("getting cached data")
                return(cm)
        }
        data <- x$get()
        cm <- solve(data, ...)
        x$setcm(cm)
        cm
}
