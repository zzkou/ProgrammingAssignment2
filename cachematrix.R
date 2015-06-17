## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
#       set the value of the matrix
#       get the value of the matrix
#       set the value of the inverse matrix
#       get the value of the inverse matrix
#       keep track whether the matrix has "changed"
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        changed <- FALSE
        set <- function(y) {
                if(!identical(x,y)) {
                        x <<- y
                        changed <<- TRUE
                        i <<- NULL
                }
        }
        get <- function() x
        setinverse <- function(inverse) {
                changed <- FALSE
                i <<- inverse
        }
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             changed = changed)
        
}


## Write a short comment describing this function
#       return cached inverse if the matrix in unchanged and its inverse has already been calculated
#       calculates the inverse, and save it in the cache via setinverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i) & !x$changed ) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
