makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
   

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
} 
tstVec <- makeVector(c(1,2,3,4,5,6,7.77))
 
rslt <- cachemean(tstVec)
rslt <- cachemean(tstVec)
rslt <- cachemean(tstVec)
message( "Result is ", rslt)
