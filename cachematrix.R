## mackeCacheMatrix caches the inverted matrix
## cacheSolve function checks for inverse in cache ad if not found then calculates it and caches



makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
          x <<- y
          m <<- NULL
        }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
       message("getting cached data")
       return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setinverse(m)
    m
}
