## two functions. 
## 1. Creates a cache of the inverse of a matrix 
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse matrix value
        inv <- NUL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        set_inverse <- function(inv_input) inv <<- inv_input
        ## get the value of the inverse
        get_inverse <- function() inv
        ## return a list of all the above functions
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check for cached inverse, if there - return it
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ## if not, get the matrix
        data <- x$get()
        ## calculate the inverse
        inv <- solve(data, ...)
        ## cache the inverse
        x$set_inverse(inv)
        ## return the result
        inv
}
