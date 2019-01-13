## 
##  This program will get the inverse of a matrix.
##  If the inverse is already available in the global cache,
##  then the value is retrieved from the cache.
##  If not, it is calculated and put in the cache.
##

## Write a short comment describing this function

## This function converts the input data into a list,
## and defines operators that can act on the data.
## It also caches the initial data provided as input.
## It only canches the data for consective application of the
## same input.
##

makeCacheMatrix <- function(x = matrix()) {
	cached_inverse <- NULL

	## Globally cache the input data.
	set <- function(y) {
		cached_matrix <<- y
		cached_inverse <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) cached_inverse <<- inverse
	get_inverse <- function() cached_inverse

	## The follwoing step converts the inpout into a list,
	## so that the operators defined here can be used on the input.
	## operartors like "$" work only on data frames and lists
	## because these have named members.

	list(set = set, get = get,
		set_inverse= set_inverse,
 		get_inverse = get_inverse)
}


## Write a short comment describing this function

## This function will calculate the inverse of the matrix if
## the inverse does not already exist in the global namespace.
##

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'

	m_inv <- x$get_inverse()

	if(!is.null(m_inv)) {
		message("Getting ccached inverse of Matrix")
		return(m_inv)
	}

	data <- x$get()

	## Create a diagonal identity matrix of same size as input
	
	I = diag(nrow(data))

	## Solve a matrix equation of the form Ax = b
      ## Here, we are trying to solve for A inverse in
	## A %*% A_inv = I, with data <- A
  
	cached_inverse <- solve(data, I)
	x$set_inverse(cached_inverse)
	cached_inverse
}
