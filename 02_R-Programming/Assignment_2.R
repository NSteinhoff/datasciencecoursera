# Example
makeVector <- function(x = numeric()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmean <- function(mean) m <<- mean
	getmean <- function() m
	list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...){
	m <- x$getmean()
	if(!is.null(m)){
		message("getting chached data")
		return(m)
	}
	data <- x$get()
	m <- mean(data, ...)
	x$setmean(m)
	m
}

# My version: Does not require a second function. The caching is included in the getMean function of the cached vector. Works with additional options supplied to the getMean function for NA removal or trimming.
makeCachedVector <- function(x = numeric()){
	m <- NULL
	args <- NULL
	
	set <- function(y){
		x <<- y
		m <<- NULL
		args <<- NULL
	}
	get <- function() x
	getMean <- function(...){
		if(is.null(m) || !identical(args, list(...))){
			m <<- mean(x, ...)
			args <<- list(...)
		} else {
			message("getting chached mean")
		}
		m
	}
	list(set = set, get = get, getMean = getMean)
}

## Assignment
# Example
makeCacheMatrix <- function(x = matrix()){
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheInverse <- function(x, ...){
	i <- x$getInverse()
	if(!is.null(i)){
		message("getting chached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}

# My version: Does not require a second function. The caching is included in the getInverse function of the cached matrix. Works with additional options supplied to the getInverse function.
makeAutoCachedMatrix <- function(x = matrix()){
	i <- NULL
	args <- NULL
	
	set <- function(y){
		x <<- y
		i <<- NULL
		args <<- NULL
	}
	get <- function() x
	getInverse <- function(...){
		if(is.null(i) || !identical(args, list(...))){
			i <<- solve(x, ...)
			args <<- list(...)
		} else {
			message("getting chached inverse")
		}
		i
	}
	list(set = set, get = get, getInverse = getInverse)
}

