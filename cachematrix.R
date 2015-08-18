## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object 
## that can cache its inverse.
##
## usage: 
##	a <- makeCacheMatrix()
##	c=rbind(c(1, -1/4), c(-1/4, 1))
##	c <- rbind(c(1, -1/4), c(-1/4, 1))
##	a$set(c)
##	a$get()
##	a$setinverse(solve(c))
##	a$getinverse()
##
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
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
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
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

