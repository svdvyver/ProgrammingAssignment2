## Function will cache the inverse of a matrix
## to avoid computing it repeatedly if not needed

makeCacheMatrix <- function(x = matrix()) {
	## Create a matrix object that is able to cache its inverse
		i <- NULL				# Ensure the variable is empty
		set <- function(y) {	# Create a list to store values
			x <<- y 			# Link to object
			i <<- Null 			# Holds the inverse
		}
		get <- function() x
		setinvers <- function(solve) i <<- solve
		getinvers <- function() i
		list(set = set, get = get,
		setinvers = setinvers,
		getinvers = getinvers)
}

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
		## Retrieve cached value if it exits
		i <- x$getinvers()
		if(!is.null(i)) {
			message("retrieve cached data")
			return(i)
		}
  
		## Compute value
		data <- x$get()
		i <- solve(data)
		x$setinvers(i)
		i
 }