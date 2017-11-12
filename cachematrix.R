## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                # inverse (m) is set to "NULL"
        set <- function(y) {                     # setter for matrix x
                x <<- y
                m <<- NULL
        }
        get <- function() x                      # getter for matrix x
        setmean <- function(solve) m <<- solve   # setter for the inverse
        getmean <- function() m                  # getter for the inverse
        list(set = set, get = get,               # creating a list
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getmean()                        # "look if there is already an inverse"
        if(!is.null(m)) {                       # "if there is an inverse, use it"
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         # "otherwise take the matrix"
        m <- solve(data, ...)                   # "calculate the inverse"
        x$setmean(m)                            # "and save it"
        m
}
