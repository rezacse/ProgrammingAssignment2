## I am going to learning about lexical scoping 
## to reduce the costly function(inverse a matrix) everty time

## take matrix as input and contain that for further operation
## also initialize some methods 

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
          x <<- y
          inversematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(im) inversematrix <<- im
        getinverse <- function() inversematrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## read the cache data to check is inverse matrix already
## calculate or not? 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
          message("getting cached data")
          return(inversematrix)
        }
  
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        return(inversematrix)
}
