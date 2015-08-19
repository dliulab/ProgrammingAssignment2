## This is the Assignment 2 for R Programming.
## The following functions are implemented:
##
##	makeCacheMatrix(): a factory function for creating a caching-enabled matrix.
##	cacheSolve(): calculates & caches an inverse matrix
##	inverse(): a "privarte" function that calculates an inverse matrix

## This function creates a special "matrix" object 
## that can cache its inverse.
##
## usage: 
##	a <- makeCacheMatrix()
##	c=rbind(c(1, -1/4), c(-1/4, 1))
##	c <- rbind(c(1, -1/4), c(-1/4, 1))
##	a$set(c)
##	a$get()
##	a$getinverse()
##
makeCacheMatrix <- function(x = matrix()) {
	## using m to store the matrix value
	## using im to store the inverse matrix 
	im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() {
		x
	}
        getinverse <- function() {
        	if(!is.null(im)) {
                	message("getting cached data")
                	return(im)
        	}
        	message("caching data")
        	im <<- inverse(x)
		return(im)
	}
        list(set = set, get = get,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then cacheSolve
## returns the inverse matrix from the cache.
##
## Usage:
##	c=rbind(c(1, -1/4), c(-1/4, 1))
##	a <- makeCacheMatrix()
##	c <- rbind(c(1, -1/4), c(-1/4, 1))
##	a$set(c)
##	a$get()
##	cacheSolve(a)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	message("Calling cacheSolve() ...")
        x$getinverse()
}


## This function calculates the inverse matrix of a square matrix.
## If the argument is not matrix, or the input matrix is NULL or 
## the input is not a square matrix then a NULL value is returned.
## TODO: could return NaN instead of NULL.
## 
## Usage:
##	inverse(rbind(c(1, -1/4), c(-1/4, 1)))

inverse <- function(x, ...) {
	if (is.null(x)) {
		return(NULL)
	}
	if (!is.matrix(x)) {
		return(NULL)
	}
	if (nrow(x) != ncol(x)) {
		return(NULL)
	}
	solve(x, ...)		
}

