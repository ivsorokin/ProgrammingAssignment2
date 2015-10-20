# Constructs the cache to hold the inverse of x
makeCacheMatrix <- function(x = matrix())
{
	invs <- NULL	# holds the inverse of x
	
	set <- function(y)
	{
		x <<- y
		invs <<- NULL
	}
	
	get <- function() x
	setinvs <- function(i) invs <<- i
	getinvs <- function() invs
	list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


# Get the inverse from cache.
# If it's not in the cache yet, put it there.
cacheSolve <- function(cache, ...)
{
	invs <- cache$getinvs()
	# put in the cache
	if (is.null(invs))
	{
		data <- cache$get()
		invs <- solve(data)
		cache$setinvs(invs)
	}
	
	invs	
}