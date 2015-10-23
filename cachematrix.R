makeCacheMatrix <- function(p = matrix()) {
    inv <- NULL
    set <- function(q) {
        p <<- q
        inv <<- NULL
    }
    get <- function() p
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function assumes that the matrix is always nondegenerated.
cacheSolve <- function(p, ...) {
    inv <- p$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- p$get()
    inv <- solve(data)
    p$setinverse(inv)
    inv
}
##source("cachematrix.R")
##> p<- rbind(c(1,3),c(4,2))
##> z<- makeCacheMatrix(p)
##> z$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    4    2
##> cacheSolve(z)
##     [,1] [,2]
##[1,] -0.2  0.3
##[2,]  0.4 -0.1
## hopely it is rigth
