## As matrix inversion is usually a costly computation it is better to
## cache the inverse of a matrix rather than compute it repeatedly .
## So these two functions  cache the inverse of a matrix together.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## Creates an empty matrix at first pass. Has 4 sub functions, 2 to retrieve and set
## the orijinal matrix, 2 to retrieve and set the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                             # Create an empty cache 
        set <- function(y) {                                  # pass input matrix to setter 
                x <<- y                 
                m <<- NULL                                    # empty cache in parent
        }
        get <- function() x                                   # retrieve matrix
        setinverse <- function(inverse) m <<- inverse         # carry inverse to cache in parent
        getinverse <- function() m                            # retrieve  cached inverse   
        list(set = set, get = get,                
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## A test  is made  if the inverse is already computed and cached
## in order to avoid inversing  repeatedly. Accordingly
## either returns the cash or computes the inverse



cacheSolve <- function(x, ...) {
        m <- x$getinverse()                                  # check if inv is cached before
        if(!is.null(m)) {                                    # if yes return the cached data
           message("getting cached data")
           return(m)
        }
       data <- x$get()                                       # else take .....
       m <- solve(data, ...)                                 # the inverse 
       x$setinverse(m)                                       # put the inverse in cache  
       m
}

