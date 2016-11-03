# Author: Herson Melo
# E-mail: hersonpc@gmail.com

###############################################################################
#
# These functions calculates the inverse of a matrix, keeping the cached result
# avoiding a new computational cost, if it is requested in succession.
#
# https://en.wikipedia.org/wiki/Invertible_matrix
#
###############################################################################



# The "makeCacheMatrix" create a new object with the ability to save a variable
# result in cache and returns it. 

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# The "cacheSolve" function consumes a this previous object and then solve the
# equation, but before solving the equation, it checks for a cache to decide if
# will return the cached value instead of processing again.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    ## Return a matrix that is the inverse of 'x'
    return(s)
}


#Tests
cm <- makeCacheMatrix()
cm$set(matrix(c(3,2,1,1), ncol=2, byrow = T))
cacheSolve(cm)
cacheSolve(cm)
