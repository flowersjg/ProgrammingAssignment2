## Functions to calculate and cache the inverse of a matrix

## Creates a matrix of functions for caching the 
## inverse of the input matrix.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	matrix(
		c(set, get, setinverse, getinverse),
		nrow=2,
		ncol=2
	)
}


## Fetches the inverse of the "matrix" produced 
## by makeCacheMatrix either 1) from cache or
## 2) by calculating and caching it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x[[2,2]]()
        if(!is.null(i)) {
        	message("getting cached inverse")
        	return(i)
        }
        data <- x[[2, 1]]()
        i <- solve(data, ...)
        x[[1, 2]](i)
        i
}