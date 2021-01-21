## We define a function makeCacheMatrix that creates special matrix objects that can save in the cache
## their inverse. The inverse can than be recalled without having to compute it every time it is needed.

## The following function takes a function and creates a new object, a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
