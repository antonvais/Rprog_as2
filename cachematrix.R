## makeCacheMatrix creates a vector(list) of 4 functions:
# get
# set
# getinv
# setinv



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
  
}


# cacheSolve calculates the inverse matrix from the vector created by makeCacheMatrix
# It will use cached value if calculated at least once.

#Example of use(you need to add the file in your workind dir):
#       source("cachematrix.R")
#       mat <- matrix(1:4,2,2)
#       vec_tmp <- makeCacheMatrix(mat)
#       matInv <- cacheSolve(vec_tmp)
#       matInv2 <- cacheSolve(vec_tmp) 
##matInv2 will be identical to matInv, but received from cache(with corresp msg)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
}
