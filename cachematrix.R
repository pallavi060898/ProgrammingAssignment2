
# special vector to call list
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        inverse <- solve(x)
        setinverse <- function(inverse) i <<- inverse
        getmean <- function() i
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}


## showing cached data

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(x)
        x$setmean(i)
        i

}
