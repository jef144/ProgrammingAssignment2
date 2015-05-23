## Functions to invert a matrix, and also preseve state in form 
## of a cache of the inverted matrix
## Subseqent calls to cacheSolve will return the cacched value, saving time
##jef144 May 2015
 
#Builds an object containing getter and setter functions plus state
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                #Super assignement 
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
   
#References the object and returns the inverted matrix, and also caches it
#Returns on SUBSEQUENT invocations the cached inverted matrixk
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
} 
 
#Exercise the functions.  See if the message is received on tries 2 and 3
tstInverse <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
 
rslt <- cacheSolve(tstInverse)
rslt <- cacheSolve(tstInverse)
rslt <- cacheSolve(tstInverse)
str(rslt)
