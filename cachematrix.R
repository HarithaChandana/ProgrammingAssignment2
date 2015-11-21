## The following functions are used to cache the inverse of a matrix

## makeCacheMatrix is used to set and get the value of a matrix 
## as well as the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) inverse <<- NULL
	getinverse <- function() inverse
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## CasceSolve function returns the inverse of a matrix.
## First it checks if the inverse is available, if yes, it gets that result
## else, it then computes the inverse of the matrix and gets the result.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data.")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse

}
