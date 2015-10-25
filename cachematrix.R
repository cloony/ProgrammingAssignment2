## By Patrik Hlavac
## Two functions based on assignment example of vector caching
## first function defines new enhanced matrix
## second function check if inversion was done previously and return inv.


## initialize new matrix with enhanced options

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(solve) inv <<- solve	
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## check makeCacheMatrix data type value and solve matrix
## depending on previously done calculation

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)) {
		message("data cache is available")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInv(inv)
	inv
}

## Example
## A <- matrix(c(1,1,3,2,5,6,7,4,9),3,3)
## a <- makeCacheMatrix(A)
## A
## cacheSolve(a)
## cacheSolve(a)


