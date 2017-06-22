## First of all, define your matrix
# random can be done by
alpha <- matrix(rnorm(9),3,3)

## than you'll work with "my_matrix$set(alpha)"
## functions will take your matrx and set it's inverse 
## and save it in cache
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## This one will retrieve the matrix inverse from cache if already calculatet. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cache data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
