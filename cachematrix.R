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
                #object inv holds the cached inverted matrix and is initially null
                inv <<- NULL
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
        inv <- x$getinverse()
        matInput = x$get() 
        
        if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
          }

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
} 
  
#Exercise the functions.  See if the message is received on tries 2 and 3
tstInverse <- makeCacheMatrix( matrix(c(1:4)   , nrow=2  ))
    
 
                              
#First call should take longer and set the chached variable
rslt <- cacheSolve(tstInverse)   
#Second call should give message "getting cached data" and run faster
rslt <- cacheSolve(tstInverse) 
  
rslt   
