## cachematrix.R, 3/22/2015
## Definition of two functions, one generating an object capable of storing a matrix and a cached
## inverse of this matrix. The other function simply computes the inverse providing that it was
## not computed already (= cached).
##
## Example:
##  matrix <- matrix(c(3,0,0,0,2,0,0,0,1), nrow=3, ncol=3)  # 3x3 matrix ()
##  obj <- makeCacheMatrix()                                # initialize "empty" object
##  obj$set(matrix)                                         # storing matrix into the object
##  obj$get()                                               # reading the stored matrix
##  obj$setinverse(solve(matrix))                           # storing the inverse 
##  obj$getinverse()                                        # reading the stored inverse
##  cacheSolve(obj)                                         # prints cached data
##
## Caveats:
##  - no checking whether the matrix is regular or not.
##  - fails for singular matrices


## This function generates a set of closures, i.e. a set of internal functions. The objective
## was to have the ability of storing and reading a matrix together with storing and reading
## its inverse. Upon calling makeCacheMatrix, a list with these internal functions is returned.
## 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    list(
        set = function(y) {
            x <<- y
            i <<- NULL
        },
        get = function() x,
        setinverse = function(inverse) i <<- inverse,
        getinverse = function() i
    )
}


## Return a matrix that is the inverse of 'x'. Use the getters defined in makeCacheMatrix in order
## to read the data stored in the object. The setters, in fact, only the 'setinverse', are used
## in the event of running the function over an object which already has the inverse computed.
##
cacheSolve <- function(x, ...) {
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