## Background
##     Matrix inversion is usually a costly computation and 
##     there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Purpose:
##     Two functions are created to solve the inverse of a matrix and to cache it.

## makeCacheMatrix: 
##     to create a special "matrix" object that can cache its inverse.
## cacheSolve:
##     to compute the inverse of the special "matrix" returned by makeCacheMatrix above.
##     If the inverse has already been calculated (and the matrix has not changed),
##     then cacheSolve retrieves the inverse from the cache.
##     The matrix supplied should be invertible.

## usage
## example
## a <- matrix(rnorm(9,1,2),3,3) # set up matrix a
## c <- solve(a) # solve the inverse of a
## b <- makeCacheMatrix(a) # call the first function
## b$get() # should be the same as matrix a
## cacheSolve(b) # the inverse of a, same as c, already in the cache
## b$getMat() # check the cached inverse of a
## cacheSolve(b) # the cached inverse of a without solving it again

## makeCacheMatrix: 
## 1.  set      set the matrix
## 2.  get:     get the matrix
## 3.  setMat:  set the inverse of the matrix
## 4.  getMat:  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(matrx) {
		x <<- matrx
		m <<- NULL
	}
	get <- function() x
	setMat <- function(matrx) m <<- matrx
	getMat <- function() m
	list(set = set, get = get,
		setMat = setMat,
		getMat = getMat)
}


## This function returns the inverse of the matrix
##     by either solving the matrix
##     or getting the cached inverse

cacheSolve <- function(x, ...) {
	m <- x$getMat()
	if (!is.null(m)) {
		message("cached inverse matrix")
		return(m)
		## Return the cached inverse
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setMat(m)
	m
	## Return a matrix that is the inverse of 'x'
}
