# General formula for inverse matrix: A^-1 = 1/det(A)*adj(A) (Cramer's Rule)

# For inverting a matrix without importing any libraries, the following formula
# can b applied (Following Cramer's Rule):
# b <- matrix(,nrow=n,ncol=n)
#   for (i in 1:n) {
#     for (j in 1:n) {
#       b[i, j] <- (-1)^(i+j) * det(A[-j, -i]) / detA
#   }
# }
# b
# Or it can be solved by using R's solve() function




# R functions used: solve(x)        (square matrices)
# This function defines the function solve() that calculates the inverse matrix
# It creates a List containing 4 functions: get, set ,setinverse and setinverse.
# The next function cacheSolve() reads upon the cached Matrix in the List, calculates its inverse,
# and re-stores the value in the setinverse function. If the function cacheSolve is recalled, it will chech if,
# for that matrix, there is already a solved result, if it does, the function cacheSolve outputs the message
# "getting cached data" and the result, without recalculating the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # cache results
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # Check if there is a cached inverse matrix
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    # output result
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
